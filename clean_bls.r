################################################################################
################################################################################
################################################################################
###
### Title: Clean BLS Data
###
### Andreas Santucci
###
### Date: April 2017
###
### Inputs: 'raw_data/bls/20[xx].q1-q4.by_area'
###         'tmp_data/population_by_MSA_year.RData'
###
### Output: 'tmp_data/bls/20[xx]_[suffix].csv'
###         'tmp_data/bls.RData'
###
### Dependencies: data.table, ggmap, magrittr, parallel
###
################################################################################
################################################################################

### Notes and data-sources.
  # http://www.nciaa.com/content.aspx?page_id=22&club_id=160641&module_id=29898

##################################################
### Set up Workspace
##################################################

require(hangover)

require(data.table)
require(ggmap)
require(magrittr)

##################################################
### Scrape Data.
##################################################

### Loads raw data. Subsets based on regex. Aggregate using FUN. Save using suffix.
hangover::CleanBLS(party.regex = c('sound recording', 'music publisher', 'musical group'),
                   suffix = 'studios_and_artists', FUN = sum)

hangover::CleanBLS(party.regex = c('alcohol', 'drinking'),
                   suffix = 'alcohol_and_drinking', FUN = sum)

##################################################
### Piece together BLS data.
##################################################

load(file = 'tmp_data/studios_and_artists.RData')
musicians <- copy(dt)
rm(dt)
setnames(musicians, 'variable', 'nmusicians')

load(file = 'tmp_data/alcohol_and_drinking.RData')
drinking <- copy(dt)
rm(dt)
setnames(drinking, 'variable', 'ndrinks')

### Bring together musicians and drinking establishments.
bls <- merge(musicians, drinking, on = intersect(names(musicians), names(drinking)))
bls[, area.title := gsub(' msa$', '', area.title, ignore.case = T)]

load(file = 'tmp_data/population_by_MSA_year.RData')
bls <- merge(bls, pop, by = c('area.title', 'year'), all.x = T)

save(bls, file = 'tmp_data/bls.RData')
