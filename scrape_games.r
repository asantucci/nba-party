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
### Inputs: NA
###
### Dependencies: data.table, magrittr, XML
###
################################################################################
################################################################################

##############################
### Set up workspace, define functions.
##############################
require(data.table)
require(magrittr)
require(XML)

scrapeMonth <- function(year, month) {
    tryCatch(paste0('http://www.basketball-reference.com/leagues/NBA_',
                    year, '_games-', month, '.html') %>%
             readHTMLTable(., stringsAsFactors = F) %>%
             `[[`('schedule'),
             error = function(e) return(NULL))
}

scrapeYear <- function(year) {
    season <- c(month.name[10:12], month.name[1:4]) %>% tolower
    lapply(season, scrapeMonth, year = year) %>% rbindlist
}

##############################
### Scrape data.
##############################
years  <- 2011:2017

data <- lapply(years, scrapeYear) %>% rbindlist

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
