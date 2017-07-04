#' Fetch a character vector of links to boxscore data.
#'
#' Simple function to get a listing of links pointing to the BoxScores
#' for all games in a given season.
#' @param year An integer describing which year of data to scrape.
#' @keywords get, links, boxscore
#' @export
#' @examples
#' getLinksToMLBBoxScore(2012)
getLinksToMLBBoxScore <- function(year) {
    pg <- paste0('http://www.baseball-reference.com/leagues/MLB/',
                 year, '-schedule.shtml')
     htmlParse(pg) %>%
         xpathSApply(., path = '//a[@href]', xmlGetAttr, name = 'href') %>%
         grep("(boxes/[A-Z]{3}/[A-Z]{3})|(previews/[0-9]{4}/)", ., value = T) # Fetch both past and future games (for betting)
}
