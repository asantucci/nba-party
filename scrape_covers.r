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
require(data.table)
require(XML)

##############################
### Define Functions
##############################
scrapeGame <- function(link) {
    text <- readLines(link, warn = F)
    date <- grep("\\<Date[^(]", text, value = T) %>% gsub('^.*- (.*)<br>', '\\1', .)
    event.id <- gsub('^.*boxscore([0-9]+).html$', '\\1', link)
    lines <- paste0('http://www.covers.com/odds/linehistory.aspx?eventId=', event.id, '&sport=NBA')
    lines <- readHTMLTable(lines, header = T, which = 1)[1, ] # Just grab one entry for now.
    data  <- data.frame(date = date, lines)
    setnames(data, c('date', 'line.ts', 'line', 'ou'))
    return(data)
}

scrapeCoverLines <- function(date) {
    ### Bring up a listing of games played on a particular day.
    pg <- paste0('http://www.covers.com/sports/NBA/matchups?selectedDate=', date)
    doc <- htmlParse(pg)
    ### Each entry in games.played is a link to a detailed score-report.
    games.played <- xpathSApply(doc, "//a/@href") %>% grep('boxscore', ., value = T)
    games.stats <- readHTMLTable(doc)
    teams <- t(sapply(games.stats, function(x) x[[1]] %>% as.character))
    colnames(teams) <- c('away', 'home')
    rownames(teams) <- NULL
    lines <- lapply(games.played, scrapeGame) %>% rbindlist
    data <- cbind(teams, lines)
    return(data)
}

##############################
### Example
##############################
scrapeCoverLines('2015-3-10')
    
