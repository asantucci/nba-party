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
### Dependencies: 
###
################################################################################
################################################################################

require(data.table)
require(magrittr)
require(RCurl)
require(parallel)
require(XML)

### This function simply fetches a listing of dates on which games were played.
### Note that many games are played on each date, and this gets us no information
### about outcomes of each game.
getSchedule <- function(year) {
    pg <- paste0('http://www.baseball-reference.com/leagues/MLB/',
                 year, '-schedule.shtml')
    htmlParse(pg) %>% xpathSApply(., path = '//div/h3', xmlValue)
}

game.days <- lapply(2000:2017, getSchedule) %>% unlist
game.days <- as.Date(game.days, format = '%A, %B %d, %Y')
save(game.days, file = 'tmp_data/game_days_mlb.RData')

### Simple function to get a listing of links pointing to the BoxScores
### for all games in a given season.
getLinksToBoxScore <- function(year) {
    pg <- paste0('http://www.baseball-reference.com/leagues/MLB/',
                 year, '-schedule.shtml')
     htmlParse(pg) %>%
         xpathSApply(., path = '//a[@href]', xmlGetAttr, name = 'href') %>%
         grep("(boxes/[A-Z]{3}/[A-Z]{3})|(previews/[0-9]{4}/)", ., value = T) # Fetch both past and future games (for betting)
}

### We go ahead and fetch links to box-scores for 2010:2017 seasons.
links <- lapply(2010:2017, getLinksToBoxScore) %>% unlist

### This function actually fetches detailed game information, given a link to the
### game's box-score.
scrapeBoxScore <- function(link) {
    pg <- paste0('www.baseball-reference.com', link)
    tryCatch(expr = {
        pg <- getURL(pg) %>% htmlParse
        ### If the game hasn't been played yet...
        if (grepl('previews', link)) {
            game.info <- xpathSApply(pg, path = '//h1', xmlValue) %>%
                strsplit(.,  split = '[^0-9], ') %>% unlist
            teams <- game.info[1]
            months.regex <- month.name %>% paste0('(', .,  ')', collapse = '|')
            date <- Filter(function(x) grepl(months.regex, x), game.info) %>%
                gsub(' Matchup', '', .)
            return(data.frame(matchup = teams, date = date, start.time = NA, attendance = NA,
                              location = game.info[2], duration = NA))
        } else {
            teams <- readHTMLTable(pg, which = 1) %>%
                `[`(., , 2)  %>%
                as.character %>%
                paste(., collapse = ' Vs. ')
            game.info <- xpathSApply(pg, path = '//div[@class="scorebox_meta"]', xmlValue)
            game.info <- strsplit(game.info, '[\r\n\t]+') %>%
                unlist %>%
                Filter(function(x) x != '', .)
        }
        return(data.frame(matchup = teams,
                          date       = game.info[1], start.time = game.info[2],
                          attendance = game.info[3], location   = game.info[4],
                          duration   = game.info[5]))
    }, error=function(e) return(NULL))
}

### We set up a parallel cluster for scraping.
cl <- makeCluster(detectCores())
clusterCall(cl, function() {
    require(magrittr)
    require(RCurl)
    require(XML)
})

### Scrape game information.
game.info <- parLapplyLB(cl, links, scrapeBoxScore)
game.info <- rbindlist(game.info)
save(game.info, file = 'tmp_data/game_info_mlb.RData')
