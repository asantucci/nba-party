################################################################################
################################################################################
################################################################################
###
### Title: Clean MLB Covers
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

require(data.table)
require(magrittr)
require(sampling)

source('functions.r')

### A character vector of full team names.
fulls <- c('arizona diamondbacks', 'atlanta braves', 'baltimore orioles', 'boston redsox',
           'chicago cubs', 'chicago whitesox', 'cincinati reds', 'cleveland indians',
           'colorado rockets', 'detroit tigers', 'houston astros', 'kansas city royals',
           'los angeles angels', 'los angeles dodgers', 'miami marlins', 'milwaukee brewers',
           'minnesota twins', 'new york mets', 'new york yankees', 'oakland athletics',
           'philadelphia phillies', 'pittsburgh pirates', 'san diego padres', 'seattle mariners',
           'san francisco giants', 'st louis cardinals', 'tampa bay rays', 'texas rangers',
           'toronto blue jays', 'washington nationals')

##################################################
### Load raw money-lines from covers.com.
##################################################
files <- list.files('tmp_data/covers_mlb/', full.names = T)
data <- lapply(files, fread) %>% rbindlist(., fill = T)
data[, c('team', 'opponent') := tstrsplit(matchup, split = '-')]
data <- data[!is.na(Line), list(date, matchup, team, opponent, Line, score)]

##################################################
### Covers.com uses team abbreviations. Expand them.
##################################################

abbrs <- data[, unique(c(unique(team), unique(opponent))) %>% sort]
abbrs <- data.table(abbr = abbrs, team.full = fulls)

### Expand team-names.
data <- merge(data, abbrs, by.x = 'team', by.y = 'abbr', all = T)
setnames(abbrs, 'team.full', 'opponent.full')

### Expand opponent-names.
data <- merge(data, abbrs, by.x = 'opponent', by.y = 'abbr', all = T)
data[, c('matchup', 'team', 'opponent') := NULL]

### Rename variables.
setnames(data, gsub('\\.full', '', tolower(names(data))))
setcolorder(data, c('date', 'team', 'opponent', 'line', 'score'))

##################################################
### Merge in Game-Information
##################################################

load(file = 'tmp_data/game_info_mlb.RData')
game.info <- game.info[!grep("Vs\\..*Vs\\.", matchup)]

### Get nick-names for lines data, so we can merge with game info.
setnames(abbrs, c('abbr', 'team'))
abbrs[, nick := gsub('.* ([^ ]+)$', '\\1', team)]
abbrs[nick == 'jays', nick := 'blue jays']
setnames(abbrs, paste('team', names(abbrs), sep = '.'))
data <- merge(data, abbrs, by.x = 'team', by.y = 'team.team', all.x = T)
setnames(abbrs, gsub('team', 'opponent', names(abbrs)))
data <- merge(data, abbrs, by.x = 'opponent', by.y = 'opponent.opponent', all = T)

### Save an abbreviations file in case we need it later.
write.csv(abbrs, file = 'mlb_abbrs.csv', row.names = F)

### There's a difference in spacing for [color]-sox. We rectify this here.
game.info[, c('team.nick', 'opponent.nick') := tstrsplit(matchup, split = ' Vs\\. ')]
game.info[, team.nick := gsub('(.*) sox', '\\1sox', team.nick, ignore.case = T) %>% tolower]
game.info[, opponent.nick := gsub('(.*) sox', '\\1sox', opponent.nick, ignore.case = T) %>% tolower]

### Set up column names and types for the merge.
setnames(game.info, 'location', 'stadium')
game.info[, date := as.Date(date, format = '%A, %B %d, %Y')]
data[, date := as.Date(date)]
data <- merge(data, game.info, by = c('team.nick', 'opponent.nick', 'date'), all = T)
data <- data[year(date) > 2010]

##################################################
### Variable creation and column classes.
##################################################

### Create a numeric money-line variable.
data[, line := gsub(' \\(Open\\)$', '', line) %>% as.numeric]
data <- data[!is.na(line) & !is.na(matchup)]

### Set the location of the game.
##  Ex: http://www.covers.com/sports/MLB/matchups?selectedDate=2011-4-01
data[, location := opponent] ### .

### Determine the odds of winning, backed out from the money-line.
data[, odds := ifelse(line < 0, 100 / (-line + 100), line / (line + 100))]

### The line is applied to 'team', and the sign indicates whether they are favored.
##  data[, mean(team.score > opponent.score), by = sign(line)]
##  lm(I(team.score > opponent.score) ~ odds, data) %>% summary

data[, weekday := weekdays(as.Date(date))]
data[, duration := gsub('Game Duration: ', '', duration)]
data[, stadium  := gsub('Venue: ', '', stadium)]
data[, attendance := gsub('Attendance: ', '', attendance) %>% gsub(',', '', . ) %>% as.numeric]
data[, start.time := gsub('Start Time: ', '', start.time)]
data[, start.time := gsub(' +(ET)|(Local)$', '', start.time)]
data[, date := as.POSIXct(paste(date, start.time))]
data[, start.time := NULL]

##################################################
### Creating panel data-set.
##################################################
lines.dup <- copy(data)
lines.dup[, `:=`(team = opponent, opponent = team,
                 score = strsplit(score, split = '-') %>%
                     lapply(., rev) %>%
                     sapply(., paste, collapse = '-'),
                 odds = 1 - odds,
                 location = location)]

lines <- rbind(data, lines.dup)

lines[, season := year(date)]

setkey(lines, team, season, date)
### Some observations have duplicates. Can't have these.
lines <- lines[!lines[, .N, by = list(team, season, date)][N > 1]]
lines[, ndays.lgame  := c(NA, diff(as.Date(date))),   by = list(team, season)]
lines[, last.game.loc := shift(location), by = list(team, season)]
lines <- lines[ndays.lgame != 0]

##################################################
### Team Locations and Distance Traveled.
##################################################
### Here, we geocode team locations to get lat-lon, and also addresses.
## require(ggmap)
## require(sp)
## teams <- lines[, unique(team)]
## locs <- sapply(teams, geocode, simplify = F)
## locs <- do.call(rbind, locs)
## locs[['team']] <- rownames(locs)
## setDT(locs)

## dmat <- spDists(locs[, list(lon, lat)] %>% as.matrix, longlat = T) # Returns distance in kilometers.
## dmat <- dmat / 1.60934  # Convert to Miles.
## rownames(dmat) <- locs$team
## colnames(dmat) <- locs$team
## save(dmat, file = 'tmp_data/distance_matrix_mlb.RData')
load(file = 'tmp_data/distance_matrix_mlb.RData')

getDist <- function(current, last, distances)
    if (!is.na(current) && !is.na(last))
        distances[current, last]

lines[, travel.dist := getDist(location, last.game.loc, dmat), by = list(location, last.game.loc)]


lines[, c('team.score', 'opponent.score') := tstrsplit(score, split = '-')]
lines[, weekend := weekday %in% c('Saturday', 'Sunday')]

##################################################
### Musicians from BLS
##################################################
load(file = 'tmp_data/nmusician_estabs_mlb.RData')
lines <- merge(lines, musicians,
               by.x = c('season', 'last.game.loc'),
               by.y = c('season', 'team'), all.x = T)

### What does it mean to party?
setkey(lines, season, team, date)
lines[, last.game.loc := shift(location), by = list(team, season)]
lines[, lag.opponent := shift(opponent),  by = list(team, season)]
lines[, first.in.series := ifelse(is.na(lag.opponent) | opponent != lag.opponent, 1, 0)]
lines[, not.first := !first.in.series]

## t <- lines[, mean(team.score > opponent.score), by = list(location, first.in.series)][, diff(V1), by = location][order(V1)]
lines[, party := ifelse(team != location & weekend == 1, nmusicians, 0)]

glm(I(team.score > opponent.score) ~ party + odds,
         data = lines, family = 'binomial') %>% summary

save(lines, file = 'tmp_data/covers_lines_mlb.RData')

##################################################
### Causal Modeling
##################################################
### Causal Model, includes all observations up through 2016.
m <- glm(I(team.score > opponent.score) ~ party + odds + I(team == location) +
             ndays.lgame + travel.dist + weekend,
         data = lines, family = 'binomial')
summary(m)

##################################################
### Predictive Bets
##################################################

### We only include party as our source of identifying variation.
m <- glm(I(team.score > opponent.score) ~ party + odds,
         data = lines[year(date) < 2016], family = 'binomial')

### We predict on holdout data.
p <- predict(m, newdata = lines[year(date) == 2016], type = 'response')

### We chose 0.45 base on hist(p), heuristically.
idx <- which(p < .45)
p1 <- mapply(Predict, prediction = p[idx],
             vegas = lines[year(date) == 2016, odds][idx],
             MoreArgs=list(threshold = 0.01))
b <- mapply(Bet, our.prediction = p1,
            actual.outcome =
                lines[year(date) == 2016, team.score > opponent.score][idx],
            odds = lines[year(date) == 2016, odds][idx], SIMPLIFY = T)

sum(b, na.rm = T)
