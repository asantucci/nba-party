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
###
### Output: 'tmp_data/bls/20[xx]_[suffix].csv'
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

### Loads raw data. Subsets based on regex. Aggregate using FUN. Save using suffix.
hangover::CleanBLS(party.regex = c('sound recording', 'music publisher', 'musical group'),
                   suffix = 'studios_and_artists', FUN = sum)

hangover::CleanBLS(party.regex = c('alcohol', 'drinking'),
                   suffix = 'alcohol_and_drinking', FUN = sum)
