################################################################################
################################################################################
################################################################################
###
### Title: Scraping Game Days and Game Info
###
### Andreas Santucci
###
### Date: April 2017
###
### Inputs: Baseball-reference.com
###
### Dependencies: data.table, hangover, magrittr, parallel, RCurl, XML
###
################################################################################
################################################################################

require(hangover)

require(data.table)
require(magrittr)
require(RCurl)
require(parallel)
require(XML)

game.days <- lapply(2000:2017, hangover::getMLBGameDays) %>% unlist
game.days <- as.Date(game.days, format = '%A, %B %d, %Y')
save(game.days, file = 'tmp_data/game_days_mlb.RData')

### We go ahead and fetch links to box-scores for 2010:2017 seasons.
links <- lapply(2010:2017, hangover::getLinksToMLBBoxScore) %>% unlist

### We set up a parallel cluster for scraping.
cl <- makeCluster(detectCores())
clusterCall(cl, function() {
    require(magrittr)
    require(RCurl)
    require(XML)
})

### Scrape game information.
game.info <- parLapplyLB(cl, links, hangover::scrapeMLBBoxScore)
game.info <- rbindlist(game.info)
save(game.info, file = 'tmp_data/game_info_mlb.RData')
