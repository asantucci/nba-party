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

require(hangover)

require(data.table)
require(parallel)
require(magrittr)
require(XML)

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

##############################
### Scrape data
##############################

roster <- parLapplyLB(cl, teams$abbr, hangover::getNBARoster)
roster <- rbindlist(roster)
roster[, birthdate := as.Date(birthdate, format = '%b %d, %Y')]

roster <- merge(roster, teams, by.x = 'team', by.y = 'abbr', all.x = T)
roster <- subset(roster, select = -1)
roster[, year := paste(year %>% as.numeric -1, substr(year, 3, 4), sep = '-')]
setnames(roster, 'year', 'season')

save(roster, file = 'tmp_data/roster.RData')
