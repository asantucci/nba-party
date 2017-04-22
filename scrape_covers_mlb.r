################################################################################
################################################################################
################################################################################
###
### Title: SCrape Covers (MLB)
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

##############################
### Set up workspace
##############################

require(data.table)
require(magrittr)
require(parallel)
require(XML)
load(file = 'tmp_data/game_days_mlb.RData')

game.days <- Filter(function(x) x < Sys.Date() & x > as.Date('2010-11-25'),
                    x = game.days) %>% as.character

##############################
### Define functions
##############################

### Scrapes lines data for a particular day.
### At the moment, just returns one betting observation.
scrapeLines <- function(link) {
    tryCatch(expr = {
        lines <- readHTMLTable(link, which = 1)
        if (is.null(lines))
            return(data.frame("no", "lines", "data"))
        else
            head(lines, 1)
    }, error = function(e) return(data.frame("no", "lines", "data")))
}

### For a particular date,this function fetches a listing of games
### that occurred, alongside their corresponding outcome/score and line.
scrapeListingOfGames <- function(date) {
    tryCatch(expr = {
        pg <- paste0('http://www.covers.com/sports/MLB/matchups?selectedDate=', date)
        lines.links <- htmlParse(pg) %>%
            xpathSApply(., '//a/@href') %>%
            grep('linehistory', ., value = T, ignore.case = T)
        games.stats <- readHTMLTable(pg)
        teams <- sapply(games.stats, function(x)
            x[, 1] %>% as.character  %>% paste(., collapse = '-'))
        scores <- sapply(games.stats, function(x)
            x[, 'R'] %>% as.character %>% paste(., collapse = '-'))
        lines <- lapply(lines.links, scrapeLines) %>% rbindlist
        data <- data.frame(date = date, matchup = teams, lines, score = scores)
        write.csv(data, file = paste0('tmp_data/covers_mlb/', date, '.csv'),
                  row.names = F)
    }, error = function(e) return(NULL))
}

cl <- makeCluster(detectCores())
clusterCall(cl, function(x) {
    require(data.table)
    require(magrittr)
    require(XML)
})
clusterExport(cl, 'scrapeLines')

##############################
### Scrape!
##############################

RESCRAPE <- FALSE
if (!RESCRAPE) {
    scraped <- list.files('tmp_data/covers_mlb') %>%
        gsub('\\.csv', '', .) %>%
        as.character
    game.days <- setdiff(game.days, scraped)
}

parLapplyLB(cl, game.days, scrapeListingOfGames)    



