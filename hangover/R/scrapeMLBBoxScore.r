#' Scrape MLB boxscore from baseball-reference.com
#'
#' This function actually fetches detailed game information, given a link to the
#' game's box-score.
#' @param link A valid suffix of a url pointing to game information.
#' @keywords scrape, mlb, boxscore
#' @export
#' @examples
#' scrapeMLBBoxScore("/boxes/DET/DET201210280.shtml")
scrapeMLBBoxScore <- function(link) {
    pg <- paste0('www.baseball-reference.com', link)
    tryCatch(expr = {
        pg <- getURL(pg) %>% htmlParse
        ### If the game hasn't been played yet...
        if (grepl('previews', link)) {
            game.info <- xpathSApply(pg, path = '//h1', xmlValue) %>%
                strsplit(.,  split = '[^0-9], ') %>% unlist
            teams <- game.info[1]
            months.regex <- month.name %>% paste0('(', .,  ')', collapse = '|')
            date <- Filter(function(x) grepl(months.regex, x), game.info) %>%
                gsub(' Matchup', '', .)
            return(data.frame(matchup = teams, date = date, start.time = NA, attendance = NA,
                              location = game.info[2], duration = NA))
        } else {
            teams <- readHTMLTable(pg, which = 1) %>%
                `[`(., , 2)  %>%
                as.character %>%
                paste(., collapse = ' Vs. ')
            game.info <- xpathSApply(pg, path = '//div[@class="scorebox_meta"]', xmlValue)
            game.info <- strsplit(game.info, '[\r\n\t]+') %>%
                unlist %>%
                Filter(function(x) x != '', .)
        }
        return(data.frame(matchup = teams,
                          date       = game.info[1], start.time = game.info[2],
                          attendance = game.info[3], location   = game.info[4],
                          duration   = game.info[5]))
    }, error=function(e) return(NULL))
}
