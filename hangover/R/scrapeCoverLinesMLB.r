
#' Scrape money-lines data for a particular day.
#'
#' Scrape money-lines data for a particular day. At the moment, we only keep
#' the first money-line we run across.
#' @param link A url describing where game data are stored.
#' @keywords scrape, lines, mlb
#' @export
#' @examples
#'
scrapeLines <- function(link) {
    tryCatch(expr = {
        lines <- readHTMLTable(link, which = 1)
        if (is.null(lines))
            return(data.frame("no", "lines", "data"))
        else
            head(lines, 1)
    }, error = function(e) return(data.frame("no", "lines", "data")))
}

### For a particular date,this function fetches a listing of games
### that occurred, alongside their corresponding outcome/score and line.
#' Scrapes a listing of games that occured on a given date, alongside outcome/score and money-line.
#'
#' Given a date, this function attempts to scrape game outcome information along with a money-line.
#' @param date A date object.
#' @keywords scrape, mlb
#' @export
#' @examples
#' scrapeCoverLinesMLB('2010-06-15')
scrapeCoverLinesMLB <- function(date) {
    tryCatch(expr = {
        pg <- paste0('http://www.covers.com/sports/MLB/matchups?selectedDate=', date)
        lines.links <- htmlParse(pg) %>%
            xpathSApply(., '//a/@href') %>%
            grep('linehistory', ., value = T, ignore.case = T)
        ### Here, we are careful with games that got postponed (i.e. scheduled to play but no outcome).
        game.ids.with.scores <- htmlParse(pg) %>%
            xpathSApply(., '//a/@href') %>%
            grep('boxscore', ., value = T, ignore.case = T) %>%
            gsub('^.*boxscore([0-9]+)\\.html$', '\\1', .)
        game.ids.with.scores <- paste0('(', game.ids.with.scores, ')', collapse = '|')
        lines.links <- Filter(function(x) grepl(game.ids.with.scores, x), lines.links)
        games.stats <- readHTMLTable(pg)
        teams <- sapply(games.stats, function(x)
            x[, 1] %>% as.character  %>% paste(., collapse = '-'))
        scores <- sapply(games.stats, function(x)
            x[, 'R'] %>% as.character %>% paste(., collapse = '-'))
        lines <- lapply(lines.links, scrapeLines) %>% rbindlist
        data <- data.frame(date = date, matchup = teams, lines, score = scores)
        write.csv(data, file = paste0('tmp_data/covers_mlb/', date, '.csv'),
                  row.names = F)
        cat(paste0("Wrote to disk data for date: ", date, "\n"))
    }, error = function(e) return(NULL))
}
