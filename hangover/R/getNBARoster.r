#' Fetches a team roster.
#'
#' Given a particular team, this function collects last 10 years of roster data.
#' @param team A string describing which team to collect roster data for.
#' @param link A prefix to a valid url.
#' @keywords get, nba, roster
#' @export
#' @examples
#' getNBARoster('LAL') # Los Angeles Lakers
getNBARoster <- function(team, link = 'http://www.basketball-reference.com/teams/') {
    pg <- paste0(link, team)
    dc <- htmlParse(pg)
    lk <- xpathSApply(dc, '//a/@href')
    ### For now, just grab first ten years of roster data.
    lk <- grep('[A-Z]{3}/[0-9]{4}.html', lk, value = T) %>% unique %>% `[`(1:10)
    roster <- lapply(lk, function(l) {
        yr <- gsub('/teams/[A-Z]{3}/([0-9]{4}).html$', '\\1', l)
        dt <- readHTMLTable(paste0('http://www.basketball-reference.com', l), which = 1)
        setDT(dt)
        dt[, `:=`(team = team, year = yr)]
        setnames(dt, gsub(' ', '', tolower(names(dt))))
        return(dt)
    })
    roster <- rbindlist(roster)
    return(roster)
}
