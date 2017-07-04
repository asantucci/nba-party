#' For internal use only, to be used within scrapeCoverLinesNBA.
#'
#' For each game played, we scrape the outcome alongside a spread.
#' @param link A url describing where the game-outcomes are stored online.
#' @keywords scrape, game
#' @export
#' @examples
#' 
scrapeGame <- function(link) {
    tryCatch(expr = {
        text <- readLines(link, warn = F)
        ### We manually extract the date-field from HTML text.
        date <- grep("\\<Date[^(]", text, value = T) %>%
            gsub('^.*- (.*)<br>', '\\1', .)
        ### Determine the event-id from the link, such that we may scrape-lines.
        event.id <- gsub('^.*boxscore([0-9]+).html$', '\\1', link)
        lines <- paste0('http://www.covers.com/odds/linehistory.aspx?eventId=',
                        event.id, '&sport=NBA')
        ### Scrape lines, just grabbing one entry for now.
        lines <- readHTMLTable(lines, header = T, which = 1)[1, ]  # <-- One entry.
        data  <- data.frame(date = date, lines)
        setnames(data, c('date', 'line.ts', 'line', 'ou'))
        return(data)
    }, error = function(e) {
        Sys.sleep(1)
        cat(paste0('Error when downloading from: ', link, '...trying again\n'))
        scrapeGame(link)
    })
}

#' Scrape game outcomes and spreads for a given date.
#'
#' Given a date, this function queries `covers.com` for game-outcomes alongside a spread.
#' @param date A date object.
#' @param rescrape A boolean indicating whether to re-scrape raw-data if it's already
#' been downloaded.
#' @param save.path A string inidcating path to save output files.
#' @param ntries A ceiling on the number of attempts to fetch data.
#' @keywords scrape, covers
#' @export
#' @examples
#' scrapeCoverLinesNBA('2012-11-19')
scrapeCoverLinesNBA <- function(date, rescrape = F, save.path = 'raw_data/covers/', ntries = 0) {
    if (ntries > 5) return(NULL)
    if (!rescrape && file.exists(paste0(save.path, date, '.csv')))
        return(NULL)
    cat(paste0("Now trying to download covers for date: ", date, "\n"))
    ### Bring up a listing of games played on a particular day.
    tryCatch(expr = {
        pg <- paste0('http://www.covers.com/sports/NBA/matchups?selectedDate=', date)
        doc <- htmlParse(pg)
        ### Each entry in games.played is a link to a detailed score-report.
        games.played <- xpathSApply(doc, "//a/@href") %>%
            grep('boxscore', ., value = T)
        ### Games.stats contains quarter-by-quarter scores, alongside team-names.
        games.stats <- readHTMLTable(doc)
        teams <- t(sapply(games.stats, function(x) x[[1]] %>% as.character)) 
        score <- t(sapply(games.stats, function(x) x[[ncol(x)-1]])) # Pt totals.
        colnames(teams) <- c('away', 'home')
        colnames(score) <- c('away.pts', 'home.pts')
        rownames(teams) <- NULL
        ### For each game played, scrape a listing of lines.
        lines <- lapply(games.played, scrapeGame) %>% rbindlist
        data <- cbind(teams, score, lines)
        write.csv(data, file = paste0(save.path, date, '.csv'), row.names = F)
        return(NULL)
    }, error = function(e) {
        Sys.sleep(1)
        cat(paste0('Error when downloading data for date: ', date, '\n'))
        scrapeCoverLines(date, ntries = ntries + 1)
    })
}
