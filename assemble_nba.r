################################################################################
################################################################################
################################################################################
###
### Title: Assemlbe NBA Data
###
### Andreas Santucci
###
### Date: July 7th, 2017
###
### Inputs: 'tmp_data/covers_lines.RData'
###         'tmp_data/population_by_MSA_year.RData'
###         'tmp_data/espn_player_data.csv'
###
### Output: 'tmp_data/nba.RData'
###
### Dependencies: data.table, hangover, ggplot2, magrittr, stargazer
###
################################################################################
################################################################################

##################################################
### Prepare workspace.
##################################################

Sys.setlocale('LC_ALL','C')  # Needed for population data (within area.title variable)
require(hangover)

require(data.table)
require(ggplot2)
require(magrittr)
## require(sampling)
require(stargazer)
set.seed(4052017)

### Load lines data and player-minutes data.
load(file = 'tmp_data/covers_lines.RData')
load(file = 'tmp_data/bls.RData')
espn <- fread('tmp_data/espn_player_data.csv')
espn[, date := as.Date(date)]

##################################################
### Prepare NBA lines data for merge, by geolocating.
##################################################
if (file.exists('tmp_data/nba_team_locations.RData')) {
    load('tmp_data/nba_team_locations.RData')  # <-- locs.
} else {
    locs <- MyGeoCode(unique(lines$team), 'nba')
    save(locs, file = 'tmp_data/nba_team_locations.RData')
}

states <- data.table(state = state.name, state.abb = state.abb)
states <- rbind(states, data.frame(state = 'District of Columbia', state.abb = 'DC'))
locs <- locs[states, on = 'state', nomatch = 0]  # Gets rid of Toronto. That's ok!

### Attach team-location information to lines data.
lines <- lines[locs, on = 'team']  # Here, we lose Toronto (that's OK).

##################################################
### Stitch together NBA lines with BLS data.
##################################################
### Attach nba team-names to BLS data.
demog <- mapply(hangover::LookupLocation, loc = locs$locality, st = locs$state.abb, team = locs$team,
                MoreArgs = list(data = bls), SIMPLIFY = F) %>% rbindlist
setnames(demog, 'state', 'state.abb')
setcolorder(demog, c('team', 'year', 'locality', 'state.abb', 'nmusicians', 'ndrinks',
                     'population', 'area.title'))
### Merge bls data with nba lines.
setnames(demog, 'year', 'season')
demog[, season := season + 1]
nba.lines <- lines[demog, on = c('team', 'season', 'locality', 'state.abb', 'season')]

##################################################
### Exclude playoff games.
##################################################
setkey(nba.lines, season, team, date)
nba.lines <- nba.lines[, head(.SD, n=82), by=list(season,team)]

##################################################
### Define a party variable.
##################################################

HOURS <- 24

### We create a discrete indicator for party based on LA or NY
nba.lines[, party.discrete := grepl("(los angeles)|(new york)|(brooklyn)", last.game.loc) &
                nhours.lgame <= HOURS &
                last.game.loc != team]

### We also create a placebo by allowing players to rest > 24 hours.
nba.lines[, party.placebo := grepl("(los angeles)|(new york)|(brooklyn)", last.game.loc) &
                nhours.lgame > HOURS &
                last.game.loc != team]

### We then create a continuous measure of nightlife. We rescale to 0-1 so it's comparable.
nba.lines[, party := ifelse(nhours.lgame <= HOURS & last.game.loc != team, log(nmusicians+1), 0)]
nba.lines[, party := party / max(party, na.rm = T)]

##################################################
### Lag changes in possesion.
##################################################

nba.lines[, simple.date := as.character(date) %>% substr(., 1, 10) %>% as.Date]
espn <- merge(nba.lines, espn,
              by.x = c('team', 'season', 'simple.date'),
              by.y = c('team', 'season', 'date'), all.x = T)

### Estimate player fatigue, by looking at number of changes in posession.
espn[, chg.pos := three.made + fg.made + free.attempted/2 + 
           to + (fg.attempted-fg.made + three.attempted - three.made - oreb)]
setkey(espn, season, player, date)
espn[, lag.chg.pos := shift(chg.pos), by = list(season, player)]

# Since we sum by location-date we double count.
changes <- espn[, list(ttl.chg.pos = sum(chg.pos) / 2), 
            keyby = list(season, location, date)]
nba.lines <- merge(nba.lines, changes, by = c('season', 'location', 'date'))

setkey(nba.lines, season, team, date)
nba.lines[, lag.chg.pos := shift(ttl.chg.pos %>% log), by = list(season, team)]

##################################################
### Jet lag measure.
##################################################
nba.lines[, ew := sin(travel.dir %% 360 / 360 * 2 * pi)]

### For visual inspection only.
## plot(nba.lines$travel.dir, nba.lines$ew)

##################################################
### Save Data
##################################################

save(nba.lines, espn, file = 'tmp_data/nba.RData')
