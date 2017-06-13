################################################################################
################################################################################
################################################################################
###
### Title: Clean BLS Data
###
### Andreas Santucci
###
### Date: April 2017
###
### Inputs: 
###
### Dependencies: 
###
################################################################################
################################################################################

### Notes and data-sources.
  # http://www.nciaa.com/content.aspx?page_id=22&club_id=160641&module_id=29898

##################################################
### Set up Workspace
##################################################

source('functions.r') # for subsetBLS
require(data.table)
require(ggmap)
require(magrittr)
require(parallel)

cl <- makeCluster(detectCores())

clusterCall(cl, function() {
    require(bit64)
    require(data.table)
    require(magrittr)
})

MyGeoCode <- function(teams) {
    locs  <- lapply(teams, geocode, output = 'more') %>% rbindlist(., fill = T)
    setnames(locs, gsub('_', '.', names(locs)))
    locs <- locs[, lapply(.SD, as.character), .SDcols = 1:ncol(locs)]
    ### Be careful, since Washington Wizards are in Washington DC (so not a county)
    locs[, county := ifelse(is.na(administrative.area.level.2),
                            administrative.area.level.1,
                            paste(administrative.area.level.2,
                                  administrative.area.level.1, sep = ', '))]
    locs <- data.table(team = teams, county = locs$county)
    save(locs, file = 'tmp_data/team_locations.RData')
}

CleanBLS <- function(party.regex, suffix, years = 2010:2016, RESCRAPE = F) {
    ### Subset BLS data into manageable chunks.
    parLapplyLB(cl, years, SubsetBLS, party.regex = party.regex, sport = suffix)
    files <- list.files(path = 'tmp_data/bls', pattern = paste0(suffix, '\\.csv$'), full.names = T)
    bls <- lapply(files, fread) %>% rbindlist    # Load in chunks of data.
    load(file = 'tmp_data/covers_lines.RData')   # Load lines data to get listing of team-names (overkill)
    lines[, pct := NULL]
    if (RESCRAPE) MyGeoCode(unique(lines$team))  # Geocode the locations for each of our NBA teams.
    load(file = 'tmp_data/team_locations.RData')
    dt <- merge(locs, bls, by.x = 'county', by.y = 'area.title', all.x = T)  # Merge BLS with location data.
    dt <- dt[, list(variable = mean(qtrly.estabs.count %>% as.numeric)),     # Average across variables.
             by = list(county, team, year)][order(year, variable)]
    setnames(dt, 'year', 'season')
    dt[, season := as.numeric(season) %>% `+`(1)]
    save(dt, file = paste0('tmp_data/', suffix, '.RData'))
}

CleanBLS(party.regex = c('sound recording', 'music publisher', 'musical group'),
         suffix = 'nba_musicians')
CleanBLS(party.regex = c('wine', 'spirit', 'beer', 'liquor', 'drinking'),
         suffix = 'nba_drinking')

################################################################################
### All below is now old code, contained in functions above.
### Leaving it to give a sense of workflow
################################################################################

if (0) {  # Don't run any code below here. It's redundant with the above. Comments still useful.

##############################
### Subset BLS Data into manageable chunks.
##############################

# 'Audio and video media reproduction', 'Arts, entertainment, and recreation'
party <- c('sound recording', 'music publisher', 'musical group')
## party <- c('\\<alcohol beverages', 'drinking establ')
years <- 2010:2016
parLapplyLB(cl, years, SubsetBLS, party.regex = party, sport = 'nba')
## parLapplyLB(cl, years, SubsetBLS, party.regex = party, sport = 'nba_drinking')

##################################################
### Merge in BLS data with corresponding NBA teams.
##################################################

### Load BLS data.
files <- list.files(path = 'tmp_data/bls', pattern = 'nba\\.csv$', full.names = T)
## files <- list.files(path = 'tmp_data/bls', pattern = 'nba_drinking\\.csv$', full.names = T)
bls <- lapply(files, fread) %>% rbindlist

### Load Covers data (simply to get a listing of team-names, it's a bit over-kill)
load(file = 'tmp_data/covers_lines.RData')
lines[, `:=`(pct = NULL)]

##################################################
### Geocode the locations for each of our NBA teams.
##################################################
## teams <- unique(lines$team)
## locs  <- lapply(teams, geocode, output = 'more') %>% rbindlist(., fill = T)
## setnames(locs, gsub('_', '.', names(locs)))
## locs <- locs[, lapply(.SD, as.character), .SDcols = 1:ncol(locs)]

## ### Be careful, since Washington Wizards are in Washington DC (so not a county)
## locs[, county := ifelse(is.na(administrative.area.level.2),
##                         administrative.area.level.1,
##                         paste(administrative.area.level.2,
##                               administrative.area.level.1, sep = ', '))]
## locs <- data.table(team = teams, county = locs$county)
## save(locs, file = 'tmp_data/team_locations.RData')
load(file='tmp_data/team_locations.RData')

musicians <- merge(locs, bls, by.x = 'county', by.y = 'area.title', all.x = T)
musicians <- musicians[, list(nmusicians = mean(qtrly.estabs.count %>% as.numeric)),
                       by = list(county, team, year)][order(year, nmusicians)]

### We'll use lagged data (sentiment about which city has the most bars/drinking
  #  establshmnets may evolve slowly, so this is not unreasonable)
setnames(musicians, 'year', 'season')
musicians[, season := season %>% as.numeric %>% `+`(1)] 
save(musicians, file = 'tmp_data/ndrink_estabs.RData')

### Used to check that each team matches exactly one county in BLS data
  # (Except for Toronto Raptors)
## for (t in locs$county)
##     cat(paste0("Team ", t, " matched with ",
##                bls[grep(t, area.title, ignore.case = T), length(unique(area.title))], '\n'))

}


