################################################################################
################################################################################
################################################################################
###
### Title: Get Locations (for each team)
###
### Andreas Santucci
###
### Date: February 2017
###
### Inputs: 'tmp_data/spreads.RData'
###
################################################################################
################################################################################

require(data.table)
require(ggmap)
require(magrittr)
require(sp)

load('tmp_data/spreads.RData')

### Here, we geocode team locations to get lat-lon, and also addresses.
teams <- data[, unique(team)]
locs <- sapply(teams, geocode, simplify = F)
locs <- do.call(rbind, locs)
locs[['team']] <- rownames(locs)
setDT(locs)

dmat <- spDists(locs[, list(lon, lat)] %>% as.matrix, longlat = T) # Returns distance in kilometers.
dmat <- dmat / 1.60934  # Convert to Miles.
rownames(dmat) <- locs$team
colnames(dmat) <- locs$team

getDist <- function(current, last, distances)
    if (!is.na(current) && !is.na(last))
        distances[current, last]

data[, travel.dist := getDist(game.loc, last.game.loc, dmat), by = list(game.loc, last.game.loc)]

save(data, file = 'tmp_data/spreads_wdist.RData')
