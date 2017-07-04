################################################################################
################################################################################
################################################################################
###
### Title: Clean BLS Data for MLB
###
### Andreas Santucci
###
### Date: July 2017
###
### Inputs: 'raw_data/bls/20[xx].q1-q4.by_area'
###
### Output: 'tmp_data/bls/[suffix]_[sport].RData'
###         e.g. 'studios_and_artists_mlb.RData'
###
### Dependencies: data.table, ggmap, magrittr, parallel
###
################################################################################
################################################################################

require(hangover)

require(data.table)
require(ggmap)
require(magrittr)
require(parallel)

cl <- makeCluster(detectCores())

clusterCall(cl, function() {
    require(bit64)
    require(data.table)
    require(magrittr)
})

### Load raw data, subset based on party.regex, aggregate using FUN, and save using suffix/sport.
CleanBLS(party.regex = c('sound recording', 'music publisher', 'musical group'),
         suffix = 'studios_and_artists', sport = 'mlb', FUN = sum)

### Avoids rescraping data. This output used for a comparison plot in the paper.
CleanBLS(party.regex = c('alcohol', 'drinking'),
         suffix = 'alcohol_and_drinking', sport = 'mlb', FUN = sum, RESCRAPE = F)
