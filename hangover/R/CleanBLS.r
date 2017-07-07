#' A function to clean BLS data.
#'
#' Given a string describing what kinds of business patterns to look for, a labeling suffix,
#' a sport, and an aggregation function, this functional subsets each year of raw BLS
#' data such that only observations matching the party.regex are retained.
#' The data are then aggregated using the aggregation function FUN.
#' Results are saved to disk according to a labeling which uses suffix and sport.
#' @param party.regex A character vector of patterns to search for in our BLS data.
#' @param suffix A labeling suffix to be used when saving the output file.
#' @param sport A string, one of 'nba' or 'mlb'
#' @param FUN An aggregation function to be used in collapsing BLS data by locality.
#' @param years A vector of integers describing which years to collect data for.
#' @param RESCRAPE A boolean indicating whether team-locations should be rescraped.
#' @keywords BLS, clean
#' @export
#' @examples
#' # not run: CleanBLS(c('recording studio', 'musical group'), 'DJ_proxy', 'nba', sum)
CleanBLS <- function(party.regex, suffix, sport, FUN, years = 2010:2016, RESCRAPE = T) {
    ### Subset each year of BLS data to only observations matching regular expression.
    lapply(years, hangover::SubsetBLS,
           party.regex = party.regex, sport = sport, suffix = suffix)
    files <- list.files(path = 'tmp_data/bls',
                        pattern = paste0(sport, '_', suffix, '\\.csv$'), full.names = T)
    bls <- lapply(files, fread) %>% rbindlist
    ### Fetch a listing of team-names from our points-spread/money-lines data.
    if (sport == 'nba') {
        load(file = 'tmp_data/covers_lines.RData')   # Load lines data to get team-names (overkill)
        lines[, pct := NULL]
        teams <- unique(lines$team)
    } else if (sport == 'mlb') {
        abbrs <- fread(file = 'mlb_abbrs.csv')
        setnames(abbrs, c('abbr', 'full', 'nick'))
        teams <- abbrs$full
    }
    ### Geocode the location of each team within the sport, and add this to our BLS data.
    if (RESCRAPE) MyGeoCode(teams, sport)
    load(file = paste0('tmp_data/', sport, '_team_locations.RData'))  # Object loaded: locs
    bls[, area.title := gsub(' msa$', '', area.title, ignore.case = T)]
    states <- data.table(state = state.name, state.abb = state.abb)
    states <- rbind(states, data.frame(state = 'District of Columbia', state.abb = 'DC'))
    locs <- locs[states, on = 'state', nomatch = 0]
    ### We can't simply merge. We have to look for the county within the MSA.
    dt <- lapply(1:nrow(locs), function(i) { 
        matched <- bls[grepl(locs[i, locality],  area.title, ignore.case = T) &
                       grepl(locs[i, state.abb], area.title, ignore.case = T)]
        matched[, c('locality', 'state', 'team') := locs[i, list(locality, state.abb, team)]]
        return(matched)
    }) %>% rbindlist
    dt <- dt[, list(variable = FUN(qtrly.estabs.count %>% as.numeric)),
             by = list(locality, state, team, year)][order(year, variable)]
    setnames(dt, 'year', 'season')
    dt[, season := as.numeric(season)]
    ### NBA season spans new-years, e.g. change 2010-11 season --> 2011.
    if (sport == 'nba') dt[, season := season + 1]
    save(dt, file = paste0('tmp_data/', suffix, '_', sport, '.RData'))
}
