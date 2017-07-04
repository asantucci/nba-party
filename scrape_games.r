################################################################################
################################################################################
################################################################################
###
### Title: Scraping NBA Schedule
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: 'http://www.basketball-reference.com/leagues/NBA_[year]_games-[month].html'
###
### Output: 'tmp_data/game_days.RData'
###
### Dependencies: data.table, magrittr, XML
###
################################################################################
################################################################################

##############################
### Set up workspace, define functions.
##############################
require(hangover)

require(data.table)
require(magrittr)
require(XML)

##############################
### Scrape data.
##############################
years  <- 2011:2017

data <- lapply(years, hangover::scrapeBBallRef) %>% rbindlist

setnames(data, c('date', 'start.time', 'visitor', 'visitor.pts',
                 'home', 'home.pts', 'bs', 'ot', 'notes'))

data[, date := as.Date(date, format = '%a, %b %d, %Y')]
data[, `:=`(visitor = tolower(visitor), home = tolower(home))]

save(data, file = 'tmp_data/games.RData')

##############################
### Create a listing of game-days.
##############################
game.days <- unique(data$date)
game.days <- as.Date(game.days, format = '%A, %b %d, %Y')

save(game.days, file = 'tmp_data/game_days.RData')












