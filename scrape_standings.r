################################################################################
################################################################################
################################################################################
###
### Title: Scrape ESPN Team Standings
###
### Andreas Santucci
###
### Date: July 2017
###
### Inputs: 
###
### Dependencies: 
###
################################################################################
################################################################################

require(hangover)

require(data.table)
require(magrittr)
require(XML)

standings <- lapply(2005:2017, hangover::scrapeESPNStandings)
standings <- rbindlist(standings)
standings[, season := gsub('NBA Standings - ', '', season)]

standings[, team := tolower(team)]
standings[, season := gsub('-', '_', season)]
standings[, pct := as.numeric(pct)]

save(standings, file = 'tmp_data/standings.RData')
