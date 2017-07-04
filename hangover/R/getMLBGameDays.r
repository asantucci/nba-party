#' Fetch a listing of game days for a particular year of MLB.
#'
#' This function simply fetches a listing of dates on which games were played.
#' Note that many games are played on each date, and this gets us no information
#' about outcomes of each game.
#' @param year An integer describing which year of data to scrape.
#' @keywords get, gamedays, mlb
#' @export
#' @examples
#' getMLBGameDays(2015)
getMLBGameDays <- function(year) {
    pg <- paste0('http://www.baseball-reference.com/leagues/MLB/',
                 year, '-schedule.shtml')
    htmlParse(pg) %>% xpathSApply(., path = '//div/h3', xmlValue)
}
