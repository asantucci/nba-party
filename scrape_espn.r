################################################################################
################################################################################
################################################################################
###
### Title: Scrape ESPN BoxScore data
###
### Andreas Santucci
###
### Date: April 2017
###
### Inputs: 'tmp_data/game_days.RData'
###
### Output: 'raw_data/espn/[gameID].csv'
###
### Dependencies: data.table, magrittr, parallel, RCurl, XML
###
################################################################################
################################################################################

require(hangover)

require(data.table)
require(magrittr)
require(RCurl)
require(XML)
require(parallel)
cl <- makeCluster(detectCores())

load('tmp_data/game_days.RData')

### Obtain a listing of game ID's.
gameIDs <- parSapplyLB(cl, game.days, hangover::extractESPNGameIDs, simplify = F)
names(gameIDs) <- game.days
save(gameIDs, file = 'tmp_data/espn_gameIDs.RData')

### Scrape.
clusterExport(cl, varlist = 'scrapeESPNGame')
clusterCall(cl, function() {
    require(data.table)
    require(magrittr)
    require(XML)
})

clusterMap(cl, fun = hangover::scrapeESPN, gameIDs = gameIDs, date = game.days,
           .scheduling = 'dynamic')
#mapply(scrapeESPN, gameIDs = gameIDs, date = game.days)




