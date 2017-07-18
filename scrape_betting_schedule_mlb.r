################################################################################
################################################################################
################################################################################
###
### Title: Scrape MLB Schedule for 2017
###
### Andreas Santucci
###
### Date: July 17th, 2017
###
### Inputs: 
###
### Dependencies: 
###
################################################################################
################################################################################

require(data.table)
require(magrittr)

load(file = 'tmp_data/game_days_mlb.RData')

game.days <- Filter(function(x) x >= Sys.Date(), game.days)

base <- 'http://www.espn.com/mlb/schedule/_/date/'
urls <- paste0(base, game.days)

data <- sapply(urls, readHTMLTable, which = 1, simplify = F)
for (i in seq_along(data))
    data[[i]]$date <- gsub(base, '', game.days[i])

data <- rbindlist(data)
data <- data[, c(1:2, ncol(data)), with = F]
setnames(data, c('team','opponent', 'date')) # Team plays at Opponent's loc

schedule <- copy(data)
schedule[, season := year(date)]

schedule[, last.game.loc := shift(opponent), by = list(team, season)]
schedule[, lag.opponent := shift(opponent),  by = list(team, season)]


save(schedule, file = 'tmp_data/schedule_2017_07_18.RData')


