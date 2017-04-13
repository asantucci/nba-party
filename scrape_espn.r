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
### Inputs: 
###
### Dependencies: 
###
################################################################################
################################################################################

require(data.table)
require(magrittr)
require(RCurl)
require(XML)

require(parallel)
cl <- makeCluster(detectCores())

load('tmp_data/game_days.RData')

extractGameIDs <- function(date) {
    date <- as.character(date)
    pg <- paste0('http://www.espn.com/nba/scoreboard/_/date/',
                 paste0(strsplit(date, split = '-')[[1]], collapse = ''))
    xmlText <- paste(readLines(pg, warn = F), sep = '\n', collapse = '')
    m <- gregexpr(pattern = 'boxscore\\?gameId=[0-9]+', text = xmlText)
    m <- regmatches(xmlText, m)[[1]]
    gameIDs <- gsub('^.*=([[:digit:]]+)$', '\\1', m)
    return(gameIDs)
}

### Obtain a listing of game ID's.
gameIDs <- parSapplyLB(cl, game.days, extractGameIDs, simplify = F)
save(gameIDs, file = 'tmp_data/espn_gameIDs.RData')

### For each game ID, we scrape the player data.
scrapeGame <- function(gameID, date, save.path = 'raw_data/espn/') {
    pg <- paste0('http://www.espn.com/nba/boxscore?gameId=', gameID)
    doc <- htmlParse(pg)
    l <- xpathApply(doc, path = '//tbody/tr/td')
    a <- sapply(l, xmlValue)
    Extract <- function(xmlvals, starting = 1) {
        xmlvals <- xmlvals[starting:length(xmlvals)]
        browser()
        bidx <- grep("^(.{2,})\\1[A-Z]{1,2}$", xmlvals)[1]
        lidx <- grep("(DNP)|(TEAM)", xmlvals)[1]
        offset <- ifelse(xmlvals[lidx] == 'DNP', 2, 1)
        return(xmlvals[bidx:(lidx-offset)] %>% matrix(., ncol = 15, byrow = T))
    }
    try(expr = {
        away <- Extract(a, 1)
        home <- Extract(a, starting = grep("%", a)[1])
        data <- readHTMLTable(pg)[1:3]
        teams <- data$linescore[[1]] %>% as.character
        data <- rbind(cbind(away, teams[1], date %>% as.character),
                      cbind(home, teams[2], date %>% as.character))
        write.csv(data, file = paste0(save.path, gameID, '.csv'), row.names = F)
    }, error = function(e) return(NULL))
    return(NULL)
}

scrapeGames <- function(date, gameIDs, rescrape = F, path = 'raw_data/espn') {
    if (!rescrape) {
        scraped <- list.files(path) %>% gsub('\\.csv', '', .)
        gameIDs <- setdiff(gameIDs, scraped)
    }
    lapply(gameIDs, scrapeGame, date = date)
}

### Scrape.
clusterExport(cl, varlist = 'scrapeGame')
clusterCall(cl, function() {
    require(data.table)
    require(magrittr)
    require(XML)
})

clusterMap(cl, fun = scrapeGames, gameIDs = gameIDs, date = game.days,
           .scheduling = 'dynamic')
#mapply(scrapeGames, gameIDs = gameIDs, date = game.days)




