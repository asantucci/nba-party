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
################################################################################
################################################################################

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

### All the heavy lifting is done in our CleanBLS function...
source('functions.r')

CleanBLS(party.regex = c('sound recording', 'music publisher', 'musical group'),
         suffix = 'studios_and_artists', sport = 'mlb', FUN = sum)
