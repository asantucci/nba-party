################################################################################
################################################################################
################################################################################
###
### Title: Cleaning overs Data
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: 'raw_data/covers/[year]-[month]-[day].csv'
###
### Output: 'tmp_data/covers_lines.RData'
###
### Dependencies: data.table, ggmap, geosphere, magrittr, sp
###
################################################################################
################################################################################

##################################################
### Set up Workspace, Read in Raw Data
##################################################

require(hangover)

require(data.table)
require(geosphere)
require(ggmap)
require(magrittr)
require(sp)

files <- list.files('raw_data/covers', full.names = T)
lines <- lapply(files, fread) %>% rbindlist

##################################################
### Clean up date and line variables. Remove extraneous vars.
##################################################

lines[, date := as.POSIXct(date, format = '%A, %B %d %Y %I:%M%p')]

lines[, line := gsub('^(-?[0-9.]+)/.*$', '\\1', line)]
lines[, line.ts := NULL]
lines[, ou := NULL]

##################################################
### Expand team abbreviations.
##################################################

lines[, `:=`(home = tolower(home), away = tolower(away))]

abbrs <- unique(lines$home) %>% sort
fulls <- hangover::getFullTeamnames('nba')

teams <- data.frame(team = fulls %>% tolower, abbr = abbrs %>% tolower, stringsAsFactors = F)
write.csv(teams, file = 'tmp_data/team_abbreviations.csv', row.names = F)
lines <- merge(lines, teams, by.x = 'home', by.y = 'abbr', all.x = T)
setnames(lines, 'team', 'home.team')
lines <- merge(lines, teams, by.x = 'away', by.y = 'abbr', all.x = T)
setnames(lines, 'team', 'away.team')

##################################################
### Remove missing observations. Set line as numeric.
##################################################

lines <- lines[line != 'OFF', list(date, home.team, away.team, home.pts, away.pts, line)]
lines[, line := as.numeric(line)]

### Season spans the new year, i.e. starts in October ends in April.
### We semantically define the season based on their year-end. E.g. '2016-17' season gets
### labeled as 2017 season.
lines[, season := ifelse(month(date) >= 10, year(date) + 1, year(date))]

##################################################
### Did the team meet the spread?
##################################################

setnames(lines, 'home.team', 'team')
setnames(lines, 'away.team', 'opponent')
lines[, `:=`(home.pts = as.numeric(home.pts),
             away.pts = as.numeric(away.pts))]
lines[, outcome := ifelse(home.pts + line >= away.pts, 'W', 'L')]  # Ties?
lines[, score := paste(home.pts, away.pts, sep = '-')]
lines[, nominal.outcome := home.pts > away.pts %>% as.integer]
lines[, `:=`(home.pts = NULL, away.pts = NULL)]
lines[, location := team]

##################################################
### Creating a panel-data set.
##################################################

lines <- hangover::createPanelDataset(lines, 'nba')

setkey(lines, team, season, date)
lines[, nhours.lgame   := c(NA, diff(date)),              by = list(team, season)]
lines[, last.game.loc  := c(NA, lag(location)[1:.N-1]),   by = list(team, season)]
lines[, last.game.time := c(NA, lag(hour(date))[1:.N-1]), by = list(team, season)]

##################################################
### Distance Traveled.
##################################################
### Here, we geocode team locations to get lat-lon, and also addresses.
locs <- hangover::MyGeoCode(unique(lines$team), 'nba')
## load(file = 'tmp_data/nba_team_locations.RData')
locs[, `:=`(lon = as.numeric(lon), lat = as.numeric(lat))]

hangover::calculateDistances(locs, 'tmp_data/distance_matrix.RData', unique(lines$team))
load(file = 'tmp_data/distance_matrix.RData')

lines[, travel.dist := hangover::getDist(location, last.game.loc, dmat),
      by = list(location, last.game.loc)]

##################################################
### Direction Traveled.
##################################################
## load(file = 'tmp_data/nba_team_locations.RData')
## locs[, `:=`(lon = as.numeric(lon), lat = as.numeric(lat))]
### Get directions.
dirs <- matrix(nrow = nrow(dmat), ncol = ncol(dmat), dimnames = dimnames(dmat))
for (i in 1:(nrow(dirs)))
    for (j in 1:ncol(dirs))
        dirs[i, j] = finalBearing(p1 = locs[i, list(lon, lat)],
                                  p2 = locs[j, list(lon, lat)])
diag(dirs) <- NA

lines[, travel.dir := hangover::getDirection(last.game.loc, location, dirs),
      by = list(last.game.loc, location)]


lines[, dir.traveled := hangover::degToCompass(travel.dir), by = travel.dir]

##################################################
### Standings from last season
##################################################

load(file = 'tmp_data/standings.RData')
standings <- subset(standings, select = c('team', 'season', 'pct'))
standings[, pct := shift(pct, type = 'lag'), by = team]
standings[, season := paste0(substr(season, 1, 2), substr(season, 6, 7)) %>% as.integer]

lines <- merge(lines, standings, by = c('season', 'team'), all.x = T)

##################################################
### Average Age
##################################################
load(file = 'tmp_data/roster.RData')
roster[, season := gsub('-', '_', season)]
roster[, season := paste0(substr(season, 1, 2), substr(season, 6, 7)) %>% as.integer]
setkey(roster, team, season)

getAvgAge <- function(t, s, game.date, roster)
    roster[.(t, s), mean(as.Date(game.date) - birthdate)]

lines[, avg.age := getAvgAge(team, season, date, roster), by = list(team, season, date)]
lines[, avg.age := as.numeric(avg.age) %>% log]

##################################################
### Win loss record
##################################################
lines[, loss.season := cumsum(!nominal.outcome), by = list(season, team)]

##################################################
### Save Data
##################################################

lines[, c('team.pts.scored', 'team.pts.admitted') := tstrsplit(score, split = '-', fixed = T)]
lines[, `:=`(team.pts.scored   = as.numeric(team.pts.scored),
             team.pts.admitted = as.numeric(team.pts.admitted))]

save(lines, file = 'tmp_data/covers_lines.RData')
