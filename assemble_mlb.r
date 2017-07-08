################################################################################
################################################################################
################################################################################
###
### Title: Assemble MLB Data
###
### Andreas Santucci
###
### Date: July 7th, 2017
###
### Inputs: 'tmp_data/covers_lines_mlb.RData'
###         'tmp_data/bls.RData'
###
### Dependencies: data.table, hangover, ggplot2, magrittr, stargazer.
###
################################################################################
################################################################################

##################################################
### Prepare workspace.
##################################################

require(hangover)

require(data.table)
require(ggplot2)
require(magrittr)
require(stargazer)

load(file = 'tmp_data/covers_lines_mlb.RData')  # lines
load(file = 'tmp_data/bls.RData')               # bls

##################################################
### Geocoe team locations, prep for merge with BLS.
##################################################
if (file.exists('tmp_data/mlb_team_locations.RData')) {
    load('tmp_data/mlb_team_locations.RData')   # locs
} else {
    locs <- MyGeoCode(unique(lines$team), 'mlb')
    save(locs, file = 'tmp_data/mlb_team_locations.RData')
}

states <- data.table(state = state.name, state.abb = state.abb)
states <- rbind(states, data.frame(state = 'District of Columbia', state.abb = 'DC'))
locs  <- locs[states, on = 'state', nomatch = 0]  # Here, we lose Toronto (that's OK).
lines <- lines[locs, on = 'team'] 

### Attach mlb team-names to BLS data.
demog <- mapply(hangover::LookupLocation, loc = locs$locality, st = locs$state.abb, team = locs$team,
                MoreArgs = list(data = bls), SIMPLIFY = F) %>% rbindlist
setnames(demog, 'state', 'state.abb')
setcolorder(demog, c('team', 'year', 'locality', 'state.abb', 'nmusicians', 'ndrinks',
                     'population', 'area.title'))

### Merge bls data with nba lines.
setnames(demog, 'year', 'season')
mlb.lines <- lines[demog, on = c('team', 'season', 'locality', 'state.abb', 'season')]

##################################################
### Exclude playoff games.
##################################################
setkey(mlb.lines, season, team, date)
mlb.lines <- mlb.lines[, head(.SD, n=162),by=list(season,team,date)] #Exclude playoff games

##################################################
### Define a measure for party.
##################################################
### A discrete indicator.
mlb.lines[, party.discrete := grepl("(los angeles)|(new york)", last.game.loc) &
                ndays.lgame == 1 & weekend == T]

### A continuous measure.
mlb.lines[, party := ifelse(team != location & weekend == 1, log(nmusicians+1), 0)]
mlb.lines[, party := party / max(party, na.rm = T)]

### See what happens if we don't interact with weekend, i.e. no weekend effect.
mlb.lines[, party.now := ifelse(team != location, log(nmusicians+1), 0)]
mlb.lines[, party.now := party.now / max(party.now, na.rm = T)]

##################################################
### Save Data.
##################################################
save(mlb.lines, file = 'tmp_data/mlb.RData')
