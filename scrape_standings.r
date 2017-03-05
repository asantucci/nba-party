
require(data.table)
require(magrittr)
require(XML)

scrapeESPNStandings <- function(year) {
    pg <- paste0('http://www.espn.com/nba/standings/_/season/', year, '/group/league')
    dt <- htmlTreeParse(pg, useInternalNodes = T)
    season   <- xpathSApply(doc = dt, path = '//div//section//header//h1', xmlValue)
    rankings <- getNodeSet(doc = dt, path = '//div//table//tr[@class=" standings-row"]')
    team.names <- xpathSApply(doc = dt, path = '//tr[@class=" standings-row"]//span//span', xmlValue)
    fields <- xpathSApply(doc = dt, path = '//tr[@class=" standings-row"]//td', xmlValue)
    breaks <- grep(paste0('(', team.names, ')', collapse = '|'), fields)
    nfields <- unique(diff(breaks))
    stopifnot(length(nfields) == 1)
    fields <- lapply(seq_along(breaks), function(i)
        fields[breaks[i]:(breaks[i]+nfields-1)])
    fields <- do.call(rbind, fields)
    data <- data.table(fields)
    setnames(data, c('team', 'wins', 'losses', 'pct', 'gb', 'home', 'road', 'div', 'conf', 'ppg', 'opp.ppg', 'diff', 'strk', 'l10'))
    data[, team := team.names]
    data[, season := season]
    return(data)
}

standings <- lapply(2005:2013, scrapeESPNStandings)
standings <- rbindlist(standings)
standings[, season := gsub('NBA Standings - ', '', season)]

standings[, team := tolower(team)]
standings[, season := gsub('-', '_', season)]
standings[, pct := as.numeric(pct)]

save(standings, file = 'tmp_data/standings.RData')
