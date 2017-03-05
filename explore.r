################################################################################
################################################################################
################################################################################
###
### Title: Exploratory Data Analysis
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: 'tmp_data/spreads_wdist.RData', 'tmp_data/standings.RData'
###
################################################################################
################################################################################

##################################################
### Set up Workspace, load data
##################################################

require(data.table)
require(ggmap)
require(ggplot2)
require(magrittr)
require(sp)
source('functions.r')

load(file = 'tmp_data/spreads_wdist.RData')
load(file = 'tmp_data/standings.RData')

data[, season := gsub('_20', '_', season)]

standings <- subset(standings, select = c('team', 'season', 'pct'))

data <- merge(data, standings, by = c('season', 'team'), all.x = T)

### Remove playoff games (sketchy)
data <- data[data[, head(.I, 82), by = list(season, team)]$V1]

##################################################
### Define a Party Variable
##################################################

party.cities <- c("miami", "los angeles", "brooklyn", "new york", "atlanta")
party.rgx <- paste0('(', party.cities, ')', collapse = "|")

data[, party := 0]
data[, weekend := grepl("(Friday)|(Saturday)|(Sunday)", day) %>% as.integer]

### We (1) exclude people who already party, (2) check if last game played against party city
### (3) check last game was played 'away' i.e. at party city and (4) party last night
data[!grepl(party.rgx, team) & grepl(party.rgx, last.game.loc) & ndays.lgame == 1, party := 1]

data[grepl(party.rgx, team) & grepl(party.rgx, last.game.loc) &
     team != last.game.loc & ndays.lgame == 1, party := 1]

### We also consider: when the team had the day off the day before playing a party city
### data[grepl('V', location) & grepl(party.rgx, opponent) & last.game > 1, table(outcome)]

data[, .N, by = list(season, team, party)]
m <- data[party==1, .N, by = list(season, team)]
ggplot(m, aes(x = N)) +
    geom_histogram() +
    labs(title = "Number of party days by season-team", x = "Number of days", y = "Frequency")
ggsave(filename = 'figure/party_days_by_season_team.pdf')

### TODO : Look at outliers here!!
### TODO : Remove playoff games.
m <- data[grepl(party.rgx, game.loc) & !grepl(party.rgx, team),
          list(meet.spread = mean(outcome == 'W'), pct = unique(pct), .N), by = list(team, season)]
cor(m[, list(meet.spread, pct)])
ggplot(m, aes(x = pct, y = meet.spread, label = N)) +
    geom_point() +
    geom_label()

### For this plot, we should look at outliers.
m <- data[, list(meet.spread = mean(outcome == 'W'), .N), by = ndays.lgame][order(ndays.lgame)]
ggplot(m, aes(x = ndays.lgame, y = meet.spread, label = N)) +
    geom_point() +
    geom_label(nudge_y = -.01) +
    geom_hline(yintercept = 0.5) + 
    labs(x = "Number of days since last game", y = "Met the Spread",
         title = paste("Proportion of games which meet spread",
                       "as a function of rest-time",
                       "(Counts indicate number of observations used)", sep = "\n"))

m <- data[, list(meet.spread = mean(outcome == 'W'), .N), by = list(season, travel.dist = round(travel.dist, digits = -2))]
ggplot(m, aes(x = travel.dist, y = meet.spread, label = N)) +
    geom_point() +
    geom_label(nudge_y = -.01) +
    geom_hline(yintercept = 0.5) +
    facet_wrap(facets = ~season) + 
    labs(x = "Travel Distance", y = "Met the Spread",
         title = paste("Proportion of games which meet spread",
                       "as a function of travel distance",
                       "(Counts indicate number of observations used)", sep = "\n"))


m <- data[, list(meet.spread = mean(outcome == 'W')), by = list(team, season)]
ggplot(m, mapping = aes(x = meet.spread)) +
    geom_histogram(binwidth = 0.01)

data[, list(avg.win = mean(outcome == 'W'), .N), by = list(team, party)]
m <- data[, list(avg.win = mean(outcome == 'W'), .N), by = list(team, season, party)]
ggplot(data = m, mapping = aes(x = avg.win, fill = as.factor(party))) +
    geom_histogram() +
    facet_wrap(facets = ~season)

m <- data[, list(meet.spread = mean(outcome == 'W')), by = list(season, team)]
m <- merge(m, standings, by = intersect(names(standings), names(m)))
setkey(m, team, season)
m[, lpct := shift(pct, type = 'lag'), by = team]

### Notice that team-performance last-year well correlated with perf. this year.
ggplot(data = m, mapping = aes(x = lpct, y = pct)) +
    geom_point() +
    facet_wrap(facets = ~season) +
    labs(x = 'Last season\'s Win Percentage', y = 'This season\'s Win Percentage',
         title = 'Team Performance between years is well correlated.') +
    geom_label(data = m[team == 'cleveland cavaliers' & season == '2010_11'],
               label = 'Cleveland Cavs') +
    geom_point(data = m[team == 'cleveland cavaliers' & season == '2010_11'], color = 'red')

### Here, we see that the spread doesn't just take into account ability.
ggplot(data = m, mapping = aes(x = lpct, y = meet.spread)) +
    geom_point() +
    facet_wrap(facets = ~season) +
    labs(x = 'Last season\'s Win Percentage',
         y = 'Proportion of games in Current Season\nfor which spread was met',
         title = 'Team Performance between years is well correlated.')

### How often are teams meeting the spread?
hist(data[, mean(outcome == 'W'), by = list(season, team)][, V1],
     breaks = seq(0.3, 0.75, by = 0.025),
     main = "How often are teams meeting the spread?",
     xlab = "Proportion of games for which a team-year meets the spread")

save(data, file = 'tmp_data/spreads_with_standings.RData')

