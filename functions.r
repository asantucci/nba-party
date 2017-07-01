
### Function: MyGeoCode
### -------------------
### Given a listing of teams for a particular sport, this function
###   uses `ggmap::geocode` to determine a longitude/latitude for each location.
MyGeoCode <- function(teams, sport) {
    locs <- lapply(teams, geocode, output = 'more') %>% rbindlist(., fill = T)
    setnames(locs, gsub('_', '.', names(locs)))
    locs <- locs[, lapply(.SD, as.character), .SDcols = 1:ncol(locs)]
    ### Here, we use an if-else since Washington D.C. not a county...
    locs[, county := ifelse(is.na(administrative.area.level.2),
                            administrative.area.level.1,
                            paste(administrative.area.level.2,
                                  administrative.area.level.1, sep = ', '))]
    locs <- data.table(team = teams, county = locs$county)
    if (sport == 'mlb') {
        locs[county == 'Maryland', county := 'Baltimore County, Maryland']
        locs[county == 'Missouri', county := 'Clay County, Missouri']
        locs[team == 'atlanta braves', county := 'Fulton County, Georgia'] # Changes in 2017.
    }
    save(locs, file = paste0('tmp_data/', sport, '_team_locations.RData'))
}

### Function: Clean BLS
### -------------------
### Given a string describing what kinds of business patterns to look for, a labeling suffix,
###   a sport, and an aggregation function, this functional subsets each year of BLS
###   data such that only observations matching the party.regex are retained.
###   Results are saved to disk according to a labeling which uses suffix and sport.
CleanBLS <- function(party.regex, suffix, sport, FUN, years = 2010:2016, RESCRAPE = F) {
    ### Subset each year of BLS data to only observations matching regular expression.
    parLapplyLB(cl, years, SubsetBLS, party.regex = party.regex, sport = sport, suffix = suffix)
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
    load(file = paste0('tmp_data/', sport, '_team_locations.RData'))
    dt <- merge(locs, bls, by.x = 'county', by.y = 'area.title', all.x = T)
    dt <- dt[, list(variable = FUN(qtrly.estabs.count %>% as.numeric)),
             by = list(county, team, year)][order(year, variable)]
    setnames(dt, 'year', 'season')
    dt[, season := as.numeric(season)]
    ### NBA season spans new-years, e.g. change 2010-11 season --> 2011.
    if (sport == 'nba') dt[, season := season + 1]
    save(dt, file = paste0('tmp_data/', suffix, '_', sport, '.RData'))
}

### Function: SubsetBLS
### -------------------
### Subset BLS data into manageable chunks, keeping only observations we care about.
SubsetBLS <- function(year, party.regex, sport, suffix,
                      raw.path = 'raw_data/bls/', save.path = 'tmp_data/bls/') {
    ### Create a listing of files for all relevant counties, load data.
    foldr <- list.files(path = raw.path, pattern = paste0(year, '.q1'), full.names = T)
    files <- list.files(path = foldr,
                        pattern = '(county,)|(district of columbia)|(parish)',
                        full.names = T, ignore.case = T)
    bls <- lapply(files, fread, colClasses = 'character',
                  select = c('area_fips', 'year', 'qtr', 'industry_code', 'area_title',
                             'industry_title', 'agglvl_title', 'size_title',
                             'qtrly_estabs_count')) %>% rbindlist
    setnames(bls, gsub('_', '.', names(bls)))
    ### We will take our own total later, so for now we take granular data.
    bls <- bls[grep("County, NAICS 6-digit", agglvl.title)]
    ### Create regex to subset to drinking establshimensts.
    party.regex <- paste0('(', party.regex, ')', collapse = '|')
    bls <- bls[grep(party.regex, industry.title, ignore.case = T)]
    write.csv(x = bls, file = paste0(save.path, year, '_', sport, '_', suffix, '.csv'),
              row.names = F)
    rm(bls)
    gc()
    return(NULL)
}

### Function: predict
### -----------------
### Given a prediction probability (determined by our model)
###   the house-odds determined by a bookmaker, and a threshold,
###   this function places a bet whenever the expected value is positive.
###   i.e. if our prediction differs from vegas' prediction by enough, we bet.
Predict <- function(prediction, vegas, threshold) {
    if (is.na(prediction)) return(NA)
    dif <- prediction - vegas
    if (abs(dif) > threshold) {
        if (dif < 0)
            return(0)
        else
            return(1)
    }
    return(NA)
}

### Function: Bet
### -------------
### This function compares our prediction with the actual outcome of the game.
###   We allow the house bookmaker a $10 cut for each bet placed, regardless of outcome.
Bet <- function(our.prediction, actual.outcome, odds) {
    actual.outcome = as.integer(actual.outcome)
    if (is.na(our.prediction)) return(NA_integer_)
    if (our.prediction == actual.outcome) {
        if (our.prediction > 0)
            return(100 / odds - 100 - 10)
        else if (our.prediction == 0)
            return(100 / (1-odds) - 100 - 10)
    } else if (our.prediction != actual.outcome)
        return(-100)
}


Fetch <- function(team, beg, end, data, year) {
    datum <- data[beg:end]
    ### We have to be careful for data entry errors. Ex: missing delimiter.
    datum <- gsub("W\\+12 89-98", "W  +12  89-98", datum) %>%               # Separate fields.
        gsub("([0-9]{3}[OU][[:blank:]]+)(.*)", "\\1", .) %>% # Throw out playoff game (not formatted)
        strsplit('[[:blank:]]{2,}')
    datum <- Filter(function(x) !any(grepl("CAN\\.", x)), datum)
    datum <- do.call(rbind, args = datum) %>%
        data.table(.) %>% cbind(team)
}


collectData <- function(datafile) {
    data <- readLines(datafile) # 'raw_data/2011.txt')

    ### Remove blank lines.
    blanks <- grep("^[[:blank:]]*$", data)
    data <- data[-blanks]

    ### Find team names, aka starting indices. 
    beg <- grep("^\\(SUR", data) - 1     # Look one row before team-summary for team-name.
    end <- c(beg[-1] - 1, length(data))  # Find limits of data for each team.
    teams <- data[beg]

    ### This data frame describes where data start-and-stop for each team.
    teams <- data.frame(team = teams, beg = beg+2, end = end)

    data <- mapply(Fetch, team = teams$team, beg = teams$beg, end = teams$end, year = '2011',
                   MoreArgs = list(data=data), SIMPLIFY = F)

    data <- rbindlist(data)
    data[['season']] = gsub('raw_data/([0-9_]{9})\\.txt', '\\1', datafile)

    setnames(data, c('date', 'opponent', 'outcome', 'spread', 'score',
                     'location', 'ou', 'team', 'season'))
    setcolorder(data, c('season', 'date', 'team', 'opponent', 'outcome', 'spread', 'score',
                        'location', 'ou'))
    return(data)
}

## getFullTeamName <- function(needle, haystack)
##     grep(needle, haystack, value = T)
