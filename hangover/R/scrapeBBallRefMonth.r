#' Helper function to scrape one month of basketball reference game-days.
#'
#' Given a year and a particular month, this function fetches a listing of
#' game days.
#' @param year An integer describing which year of data to scrape.
#' @param month A string describing which month of data to scrape.
#' @keywords scrape, month, basketball, reference.
#' @export
#' @examples
#' scrapeBBallRefMonth(2015, 'october')
scrapeBBallRefMonth <- function(year, month) {
    tryCatch(paste0('http://www.basketball-reference.com/leagues/NBA_',
                    year, '_games-', month, '.html') %>%
             readHTMLTable(., stringsAsFactors = F) %>%
             `[[`('schedule'),
             error = function(e) return(NULL))
}
