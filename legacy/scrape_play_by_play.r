################################################################################
################################################################################
################################################################################
###
### Title: Scrape Play-by-Play data
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

require(parallel)
require(XML)
cl <- makeCluster(detectCores())

load(file = 'tmp_data/espn_gameIDs.RData')

extractGame <- function(gameID, date) {
    tryCatch(expr = {
        pg <- paste0('http://scores.espn.com/nba/playbyplay?gameId=', gameID)
        tb <- readHTMLTable(pg)
        scores <- tb[[1]]
        teams  <- paste0(scores[, 1], collapse = '-')
        ls <- sapply(tb, nrow)
        tb <- tb[ls > 32]
        data <- do.call(rbind, tb)
        data$matchup <- teams
        data$date <- date
        write.csv(data,
                  file = paste0('raw_data/play_by_play/', gameID, '.csv'),
                  row.names = F)
    }, error = function(e) return(NULL))
}

## extractPlays(gameIDs[[1]][1], names(gameIDs)[1])
## We do have 'flagrant foul'

extractPlays <- function(gameIDs, date, rescrape = F) {
    if (!rescrape) {
        scraped <- list.files('raw_data/play_by_play') %>% gsub('\\.csv', '', .)
        gameIDs <- setdiff(gameIDs, scraped)
    }
    lapply(gameIDs, extractGame, date = date) %>% rbindlist
}

clusterCall(cl, function() {
    require(XML)
    require(magrittr)
    require(data.table)
})
clusterExport(cl, 'extractGame')
clusterMap(cl, fun = extractPlays, gameIDs = gameIDs, date = names(gameIDs),
           .scheduling = 'dynamic')
#mapply(scrapeGames, gameIDs = gameIDs, date = game.days)
