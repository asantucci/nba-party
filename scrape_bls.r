################################################################################
################################################################################
################################################################################
###
### Title: Scraping BLS
###
### Andreas Santucci
###
### Date: April 2017
###
### Inputs: 
###
### Dependencies: 
###
################################################################################
################################################################################

require(XML)
require(magrittr)
require(RCurl)
require(parallel)

cl <- makeCluster(detectCores())

yrs <- 2010:2016
links <- paste0("https://data.bls.gov/cew/data/files/", yrs,
                "/csv/", yrs, "_qtrly_by_area.zip")

clusterMap(cl, download.file, url = links,
           destfile = paste0('raw_data/bls/', yrs, '.zip'))

files <- list.files(path = 'raw_data/bls/', pattern = 'zip$', full.names = T)
sapply(files, unzip, exdir = 'raw_data/bls')

##############################
### Scratch
##############################

### This code was used to extract format of 'links' as created above.
## datatoc <- 'https://www.bls.gov/cew/datatoc.htm'
## doc <- getURL(datatoc, ssl.verifypeer = F) %>% htmlParse

## links <- xpathSApply(doc, '//a[@href]', xmlGetAttr, name = 'href')
## links <- grep('/[0-9]{4}_qtrly_by_area', links, value = T)
## links <- grep('/201', links, value = T)
