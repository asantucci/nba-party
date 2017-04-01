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
### Dependencies: data.table, magrittr
###
################################################################################
################################################################################

##################################################
### Set up Workspace, Read in Raw Data
##################################################

require(data.table)
require(magrittr)
require(ggmap)
require(sp)

files <- list.files('raw_data/covers', full.names = T)
lines <- lapply(files, fread) %>% rbindlist

##################################################
### Clean up date and line variables. Remove extraneous vars.
##################################################

lines[, game.time := gsub('^.* ([0-9:APM]+)$', '\\1', date)]
lines[, date := as.Date(date, format = '%A, %B %d %Y')]

lines[, line := gsub('^(-?[0-9.]+)/.*$', '\\1', line)]
lines[, line.ts := NULL]
lines[, ou := NULL]
lines[, game.time := NULL]

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
lines[, `:=`(home.pts = NULL, away.pts = NULL)]
lines[, location := team]
lines <- lines[, list(season, date, team, opponent, location, line, score, outcome)]

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
lines[, ndays.lgame := c(NA, diff(date)), by = list(team, season)]
lines[, last.game.loc := c(NA, lag(location)[1:.N-1]), by = list(team, season)]

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

lines[, travel.dist := getDist(location, last.game.loc, dmat), by = list(location, last.game.loc)]


##################################################
### Merge in older data?
##################################################
## load(file = 'tmp_data/spreads_wdist.RData')
## data[, season := substr(season, 6, 9)]
## data[, `:=`(location = NULL, ou = NULL, ou.out = NULL, day = NULL)]
## setnames(data, c('spread', 'game.loc'), c('line', 'location'))
## lines <- rbind(lines[season > 2011], data[season <= 2011])
## setkey(lines, season, team)
## lines[, season := as.integer(season)]

##################################################
### Standings from last season
##################################################

load(file = 'tmp_data/standings.RData')
standings <- subset(standings, select = c('team', 'season', 'pct'))
standings[, pct := shift(pct, type = 'lag'), by = team]
standings[, season := paste0(substr(season, 1, 2), substr(season, 6, 7)) %>% as.integer]

lines <- merge(lines, standings, by = c('season', 'team'), all.x = T)
###lines <- lines[lines[, head(.I, 82), by = list(season, team)]$V1] ### Excluded playoff games...

##################################################
### Party
##################################################

### Define a Party Variable
lines[ndays.lgame == 1, list(meet.spread = mean(outcome == 'W'), .N), by = list(season, last.game.loc)][order(meet.spread)]
party.cities <- c('boston', 'brooklyn', 'dallas', 'los angeles')
party.rgx <- paste0('(', party.cities, ')', collapse = "|")

lines[, party := 0]
lines[!grepl(party.rgx, team) & grepl(party.rgx, last.game.loc) & ndays.lgame == 1, party := 1]

lines[, weekend := grepl("(Friday)|(Saturday)|(Sunday)", weekdays(date)) %>% as.integer]

#save(lines, file = 'tmp_data/covers_lines.RData')
##################################################
### Average Age
##################################################
load(file = 'tmp_data/roster.RData')
roster[, season := gsub('-', '_', season)]
roster[, season := paste0(substr(season, 1, 2), substr(season, 6, 7)) %>% as.integer]
setkey(roster, team, season)
getAvgAge <- function(t, s, game.date, roster) {
    roster[.(t, s), mean(game.date - birthdate)]
}
lines[, avg.age := getAvgAge(team, season, date, roster), by = list(team, season, date)]
lines[, avg.age := as.numeric(avg.age) %>% log]

#save(lines, file = 'tmp_data/covers_lines.RData')

##################################################
### Exploring data
##################################################

require(ggplot2)

hist(lines[, mean(outcome == 'W'), by = list(season, team)][, V1],
     breaks = seq(0.3, 0.75, by = 0.025),
     main = "How often are teams meeting the spread?",
     xlab = "Proportion of games for which a team-year meets the spread")

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N),
             by = list(last.game.loc, ndays.lgame)][order(ndays.lgame)]

ggplot(tmp %>% na.omit, aes(x = ndays.lgame, y = meet.spread, label = N)) +
    geom_point() +
    geom_label() + 
    geom_hline(yintercept = 0.5) + 
    facet_wrap(facets = ~ last.game.loc)

lines[party==1, .N, by = list(season, team)][, N] %>%
    hist(main = "Number of party days by season-team", xlab = "Number of days",
         ylab = "Frequency")

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N), 
            by = list(pct, party)][order(pct)]
ggplot(tmp, aes(x = pct, y = meet.spread, color = as.factor(party), label = N)) + 
    geom_label() +
    geom_smooth()

m <- lines[,#grepl(party.rgx, game.loc) & !grepl(party.rgx, team),
          list(meet.spread = mean(outcome == 'W'), pct = unique(pct), .N), by = list(team, season)]
ggplot(m, aes(x = pct, y = meet.spread)) +
    geom_point() +
    facet_wrap(facets = ~season) + 
    labs(title = "In select seasons, lagged performance not predictive of meet the spread",
         x = "Percentage of games won last season",
         y = "Proportion of games for which spread was met in current season")

m <- lines[, list(meet.spread = mean(outcome == 'W'), .N), by = ndays.lgame][order(ndays.lgame)]
ggplot(m, aes(x = ndays.lgame, y = meet.spread, label = N)) +
    geom_point() +
    geom_label(nudge_y = -.04) +
    geom_hline(yintercept = 0.5) +
    scale_x_continuous(limits = c(1, 12), breaks = 1:12, labels = 1:12) +
    labs(x = "Number of days since last game", y = "Met the Spread",
         title = paste("Proportion of games which meet spread",
                       "as a function of rest-time",
                       "(Counts indicate number of observations used)", sep = "\n"))

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N), by = list(team, party)][order(team, party)]

hist(lines$avg.age)

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N), 
            by = list(avg.age = round(avg.age, 2), party)][
    order(avg.age, party)] %>% na.omit

ggplot(tmp, aes(x = avg.age, y = meet.spread, 
                color = party %>% as.factor, label = N)) + 
    geom_point() +
    geom_label() +
    geom_smooth()

##################################################
### Toy model.
##################################################

model <- glm(outcome == 'W' ~ party + avg.age + party:avg.age + ndays.lgame + 
                 pct + weekend + I(log(travel.dist+1)) + as.factor(season),
             data = lines, family = 'binomial', subset = season > 2011 & season < 2017)
summary(model)
