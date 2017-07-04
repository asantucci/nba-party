################################################################################
################################################################################
################################################################################
###
### Title: SCrape Covers (MLB)
###
### Andreas Santucci
###
### Date: April 2017
###
### Inputs: 'tmp_data/game_days_mlb.RData'
###
### Output: 'tmp_data/covers_mlb/[date].csv'
###
### Dependencies: data.table, magrittr, parallel, XML
###
################################################################################
################################################################################

##############################
### Set up workspace
##############################

require(hangover)

require(data.table)
require(magrittr)
require(parallel)
require(XML)
load(file = 'tmp_data/game_days_mlb.RData')

game.days <- Filter(function(x) x > as.Date('2011-01-01'),
                    x = game.days) %>% as.character

##############################
### Scrape!
##############################

### Set up our cluster.
cl <- makeCluster(detectCores())
clusterCall(cl, function(x) {
    require(data.table)
    require(magrittr)
    require(XML)
})
clusterExport(cl, 'scrapeLines')

### Avoid re-scraping files, if desired.
RESCRAPE <- FALSE
if (!RESCRAPE) {
    scraped <- list.files('tmp_data/covers_mlb') %>%
        gsub('\\.csv', '', .) %>%
        as.character
    game.days <- setdiff(game.days, scraped)
}

### Scrape.
lapply(game.days, scrapeCoverLinesMLB)  # May have to run this a few times to scrape all files.


