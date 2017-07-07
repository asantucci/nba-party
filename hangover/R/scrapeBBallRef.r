#' Scrape an entire season of game-days from basketball-reference.com
#'
#' Obtains a listing of game-days for a given season.
#' @param year An integer describing which year of data to scrape.
#' @keywords scrape, basketball, reference.
#' @export
#' @examples
#' # not run: scrapeBBallRef(2015)
scrapeBBallRef <- function(year) {
    season <- c(month.name[10:12], month.name[1:4]) %>% tolower
    lapply(season, hangover::scrapeBBallRefMonth, year = year) %>% rbindlist
}
