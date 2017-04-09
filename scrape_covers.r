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
### Inputs: NA
###
### Dependencies: data.table, XML
###
################################################################################
################################################################################

##############################
### Set up Workspace
##############################

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
### Define Functions
##############################
scrapeGame <- function(link) {
    tryCatch(expr = {
        text <- readLines(link, warn = F)
        ### We manually extract the date-field from HTML text.
        date <- grep("\\<Date[^(]", text, value = T) %>%
            gsub('^.*- (.*)<br>', '\\1', .)
        ### Determine the event-id from the link, such that we may scrape-lines.
        event.id <- gsub('^.*boxscore([0-9]+).html$', '\\1', link)
        lines <- paste0('http://www.covers.com/odds/linehistory.aspx?eventId=',
                        event.id, '&sport=NBA')
        ### Scrape lines, just grabbing one entry for now.
        lines <- readHTMLTable(lines, header = T, which = 1)[1, ]  # <-- One entry.
        data  <- data.frame(date = date, lines)
        setnames(data, c('date', 'line.ts', 'line', 'ou'))
        return(data)
    }, error = function(e) {
        Sys.sleep(1)
        cat(paste0('Error when downloading from: ', link, '...trying again\n'))
        scrapeGame(link)
    })
}

scrapeCoverLines <- function(date, rescrape = F, save.path = 'raw_data/covers/', ntries = 0) {
    if (ntries > 5) return(NULL)
    if (!rescrape && file.exists(paste0(save.path, date, '.csv')))
        return(NULL)
    cat(paste0("Now trying to download covers for date: ", date, "\n"))
    ### Bring up a listing of games played on a particular day.
    tryCatch(expr = {
        pg <- paste0('http://www.covers.com/sports/NBA/matchups?selectedDate=', date)
        doc <- htmlParse(pg)
        ### Each entry in games.played is a link to a detailed score-report.
        games.played <- xpathSApply(doc, "//a/@href") %>%
            grep('boxscore', ., value = T)
        ### Games.stats contains quarter-by-quarter scores, alongside team-names.
        games.stats <- readHTMLTable(doc)
        teams <- t(sapply(games.stats, function(x) x[[1]] %>% as.character)) 
        score <- t(sapply(games.stats, function(x) x[[ncol(x)-1]])) # Pt totals.
        colnames(teams) <- c('away', 'home')
        colnames(score) <- c('away.pts', 'home.pts')
        rownames(teams) <- NULL
        ### For each game played, scrape a listing of lines.
        lines <- lapply(games.played, scrapeGame) %>% rbindlist
        data <- cbind(teams, score, lines)
        write.csv(data, file = paste0(save.path, date, '.csv'), row.names = F)
        return(NULL)
    }, error = function(e) {
        Sys.sleep(1)
        cat(paste0('Error when downloading data for date: ', date, '\n'))
        scrapeCoverLines(date, ntries = ntries + 1)
    })
}

##############################
### Scrape
##############################

### Note that we must export scrapeCoverLines since in the event of a fault,
### an individual core will need to re-call this function.
clusterExport(cl, c('scrapeGame', 'scrapeCoverLines'))
lines <- parLapplyLB(cl, game.days, scrapeCoverLines)
#lapply(game.days, scrapeCoverLines)
