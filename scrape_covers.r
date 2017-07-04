################################################################################
################################################################################
################################################################################
###
### Title: Scraping Lines Data from Covers.com
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: 'tmp_data/game_days.RData'
###
### Output: 'raw_data/covers/[date].csv'
###
### Dependencies: data.table, magrittr, parallel, XML
###
################################################################################
################################################################################

##############################
### Set up Workspace
##############################

require(hangover)
require(parallel)

cl <- makeCluster(detectCores())
clusterCall(cl, function() {
    require(data.table)
    require(magrittr)
    require(XML)
})

load(file = 'tmp_data/game_days.RData')
game.days <- Filter(function(x) x < Sys.Date() & x > '2010-11-25', game.days)
## game.days <- Filter(function(x) grepl("^201[^0]", x), game.days)
## game.days <- Filter(function(x) x < Sys.Date(), game.days)
game.days <- gsub('-0([0-9])-', '-\\1-', game.days)

##############################
### Scrape
##############################

### Note that we must export scrapeCoverLines since in the event of a fault,
### an individual core will need to re-call this function.
clusterExport(cl, c('hangover::scrapeGame', 'hangover::scrapeCoverLinesNBA'))
lines <- parLapplyLB(cl, game.days, hangover::scrapeCoverLinesNBA)
