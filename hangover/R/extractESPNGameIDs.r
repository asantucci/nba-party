#' Scrapes listing of game ID's from ESPN.
#'
#' Given a date object, this function scrapes a listing of game ID's from ESPN.
#' @param date A date object describing which date to scrape.
#' @keywords scrape, ESPN, id
#' @export
#' @examples
#' extractESPNGameIDs('2017-04-12')
extractESPNGameIDs <- function(date) {
    date <- as.character(date)
    pg <- paste0('http://www.espn.com/nba/scoreboard/_/date/',
                 paste0(strsplit(date, split = '-')[[1]], collapse = ''))
    xmlText <- paste(readLines(pg, warn = F), sep = '\n', collapse = '')
    m <- gregexpr(pattern = 'boxscore\\?gameId=[0-9]+', text = xmlText)
    m <- regmatches(xmlText, m)[[1]]
    gameIDs <- gsub('^.*=([[:digit:]]+)$', '\\1', m)
    return(gameIDs)
}
