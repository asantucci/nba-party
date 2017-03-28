################################################################################
################################################################################
################################################################################
###
### Title: Scrape NBA Rosters
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: None
###
### Dependencies: data.table, magrittr, parallel, XML
###
################################################################################
################################################################################

##############################
### Set up Workspace
##############################
require(parallel)

cl <- makeCluster(detectCores())
clusterCall(cl, function()  {
    require(data.table)
    require(magrittr)
    require(XML)
})

load(file = 'tmp_data/spreads.RData')
full.names <- data[, unique(team)]

##############################
### Create a listing of team abbreviations
##############################

### Create a data.frame of team-names and team-abbreviations
### (as used on bball-reference.com)
doc   <- htmlParse('http://www.basketball-reference.com/teams/')
links <- xpathSApply(doc, '//a/@href')
abbrs <- grep('/teams/[A-Z]{3}/$', x = links, value = T) %>%
    gsub('/teams/([A-Z]{3})/', '\\1', .) %>%
    `[`(1:30) # Grab 30 active teams.

teams <- data.table(team = full.names, abbr = abbrs)

getRoster <- function(team, link = 'http://www.basketball-reference.com/teams/') {
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

##############################
### Scrape data
##############################

roster <- parLapplyLB(cl, teams$abbr, getRoster)
roster <- rbindlist(roster)
roster[, birthdate := as.Date(birthdate, format = '%b %d, %Y')]

roster <- merge(roster, teams, by.x = 'team', by.y = 'abbr', all.x = T)
roster <- subset(roster, select = -1)
roster[, year := paste(year %>% as.numeric -1, substr(year, 3, 4), sep = '-')]
setnames(roster, 'year', 'season')

save(roster, file = 'tmp_data/roster.RData')
