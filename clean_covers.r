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
fulls <- c('Atlanta Hawks',          'Brooklyn Nets',          'Boston Celtics',    'Charlotte Bobcats',
           'Chicago Bulls',          'Cleveland Cavaliers',    'Dallas Mavericks',  'Denver Nuggets',
           'Detroit Pistons',        'Golden State Warriors',  'Houston Rockets',   'Indiana Pacers',
           'Los Angeles Clippers',   'Los Angeles Lakers',     'Memphis Grizzlies', 'Miami Heat',
           'Milwaukee Bucks',        'Minnesota Timberwolves', 'New Orleans Hornets', 'New York Knicks',
           'Oklahoma City Thunder',  'Orlando Magic',          'Philadelphia Sixers', 'Phoenix Suns',
           'Portland Trail Blazers', 'San Antonio Spurs', 'Sacramento Kings',         'Toronto Raptors',
           'Utah Jazz',              'Washington Wizards')

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

### We first need to reshape our data, since we want a panel-data set.
### I.e., for each time we want a listing of games played in a season.
### This does in fact mean that we have the same game listed twice in our data,
### but the observations will be different since the lagged variables will be different.
### I.e. the number of days since last game and distance traveled will be different for each
### of the teams.
### To do this, we simply 'reverse' each obseration, carefully.
lines.dup <- copy(lines)
lines.dup[, `:=`(team = opponent, opponent = team,
                 score = strsplit(score, split = '-') %>%
                     lapply(., rev) %>%
                     sapply(., paste, collapse = '-'),
                 line = -1 * line,
                 location = location,
                 outcome = ifelse(outcome == 'L', 'W', 'L'))]

lines <- rbind(lines, lines.dup)

setkey(lines, team, season, date)
lines[, nhours.lgame   := c(NA, diff(date)),              by = list(team, season)]
lines[, last.game.loc  := c(NA, lag(location)[1:.N-1]),   by = list(team, season)]
lines[, last.game.time := c(NA, lag(hour(date))[1:.N-1]), by = list(team, season)]

##################################################
### Distance Traveled.
##################################################
### Here, we geocode team locations to get lat-lon, and also addresses.
## teams <- lines[, unique(team)]
## locs <- sapply(teams, geocode, simplify = F)
## locs <- do.call(rbind, locs)
## locs[['team']] <- rownames(locs)
## setDT(locs)

## dmat <- spDists(locs[, list(lon, lat)] %>% as.matrix, longlat = T) # Returns distance in kilometers.
## dmat <- dmat / 1.60934  # Convert to Miles.
## rownames(dmat) <- locs$team
## colnames(dmat) <- locs$team
## save(dmat, file = 'tmp_data/distance_matrix.RData')
load(file = 'tmp_data/distance_matrix.RData')

getDist <- function(current, last, distances)
    if (!is.na(current) && !is.na(last))
        distances[current, last]

lines[, travel.dist := getDist(location, last.game.loc, dmat),
      by = list(location, last.game.loc)]

##################################################
### Direction Traveled.
##################################################
load(file = 'tmp_data/nba_team_locations.RData')
locs[, `:=`(lon = as.numeric(lon), lat = as.numeric(lat))]
### Get directions.
dirs <- matrix(nrow = nrow(dmat), ncol = ncol(dmat), dimnames = dimnames(dmat))
for (i in 1:(nrow(dirs)))
    for (j in 1:ncol(dirs))
        dirs[i, j] = finalBearing(p1 = locs[i, list(lon, lat)],
                                  p2 = locs[j, list(lon, lat)])
diag(dirs) <- NA

getDirection <- function(last.loc, cur.loc, dirs)
    if(!is.na(last.loc) && !is.na(cur.loc))
        dirs[last.loc, cur.loc]

lines[, travel.dir := getDirection(last.game.loc, location, dirs),
      by = list(last.game.loc, location)]


# https://stackoverflow.com/questions/7490660/converting-wind-direction-in-angles-to-text-words
degToCompass <- function(x) {
    val <- ((x/22.5)+.5)
    arr <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
    arr[(val %% 16)]
}

lines[, dir.traveled := degToCompass(travel.dir), by = travel.dir]


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
getAvgAge <- function(t, s, game.date, roster) {
    roster[.(t, s), mean(as.Date(game.date) - birthdate)]
}
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
lines[, `:=`(team.pts.scored = as.numeric(team.pts.scored),
             team.pts.admitted = as.numeric(team.pts.admitted))]

save(lines, file = 'tmp_data/covers_lines.RData')
