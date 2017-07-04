#' Scrape listing of game-outcomes for a given date.
#'
#' Given a date and a listing of gameID's, this function scrapes
#' data from espn.com, and saves results to the path specified.
#' @param date A string representing which date to scrape.
#' @param gameIDs A vector of integers describing gameIDs occuring on said date.
#' @param rescrape A boolean indicating whether to rescrape data that's been saved to disk.
#' @param path A string describing the output file path prefix.
#' @keywords scrape, espn
#' @export
#' @examples
#' scrapeESPN('2017-04-12', 370411122)
scrapeESPN <- function(date, gameIDs, rescrape = F, path = 'raw_data/espn') {
    if (!rescrape) {
        scraped <- list.files(path) %>% gsub('\\.csv', '', .)
        gameIDs <- setdiff(gameIDs, scraped)
    }
    lapply(gameIDs, hangover::scrapeESPNGame, date = date)
}
