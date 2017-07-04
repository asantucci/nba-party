#' Helper function to scrape a single game's outcomes from ESPN.
#'
#' For each game ID, we scrape the game outcome data.
#' @param gameID An integer describing the game id number. (To obtain this, we had to run a separate scraper).
#' @param date A string or date object describing which date to scrape.
#' @param save.path A character prefix describing where to save data.
#' @keywords scrape, ESPN, game
#' @export
#' @examples
#' scrapeESPNGame(370411122, '2017-04-12')
scrapeESPNGame <- function(gameID, date, save.path = 'raw_data/espn/') {
    # First, we download and parse HTML contents.
    pg <- paste0('http://www.espn.com/nba/boxscore?gameId=', gameID)
    doc <- htmlParse(pg)
    l <- xpathApply(doc, path = '//tbody/tr/td')
    a <- sapply(l, xmlValue)
    # We then define a helper function to extract valid games.
    Extract <- function(xmlvals, starting = 1) {
        xmlvals <- xmlvals[starting:length(xmlvals)]
        #browser()
        bidx <- grep("^(.{2,})\\1[A-Z]{1,2}$", xmlvals)[1]
        lidx <- grep("(DNP)|(TEAM)", xmlvals)[1]
        offset <- ifelse(xmlvals[lidx] == 'DNP', 2, 1)
        return(xmlvals[bidx:(lidx-offset)] %>% matrix(., ncol = 15, byrow = T))
    }
    try(expr = {
        away <- Extract(a, 1)
        home <- Extract(a, starting = grep("%", a)[1])
        data <- readHTMLTable(pg)[1:3]
        teams <- data$linescore[[1]] %>% as.character
        data <- rbind(cbind(away, teams[1], date %>% as.character),
                      cbind(home, teams[2], date %>% as.character))
        write.csv(data, file = paste0(save.path, gameID, '.csv'), row.names = F)
    }, error = function(e) return(NULL))
    return(NULL)
}
