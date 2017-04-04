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
### Inputs: 
###
### Dependencies: 
###
################################################################################
################################################################################

##################################################
### Set up Workspace
##################################################

require(parallel)

cl <- makeCluster(detectCores())

clusterCall(cl, function() {
    require(bit64)
    require(data.table)
    require(magrittr)
})

SubsetBLS <- function(year, raw.path = 'raw_data/bls/', save.path = 'tmp_data/bls/',
                      party = c('wine', 'spirit', 'beer', 'liquor', 'drinking')) {
    ### Create a listing of files for all relevant counties, load data.
    foldr <- list.files(path = raw.path, pattern = paste0(year, '.q1'), full.names = T)
    files <- list.files(path = foldr,
                        pattern = '(county,)|(district of columbia)|(parish)',
                        full.names = T, ignore.case = T)
    bls <- lapply(files, fread, colClasses = 'character',
                  select = c('area_fips', 'year', 'qtr', 'industry_code', 'area_title',
                             'industry_title', 'agglvl_title', 'size_title',
                             'qtrly_estabs_count')) %>% rbindlist
    setnames(bls, gsub('_', '.', names(bls)))
    ### We will take our own total later, so for now we take granular data.
    bls <- bls[grep("County, NAICS 6-digit", agglvl.title)]
    ### Create regex to subset to drinking establshimensts.
    party <- paste0('(\\<', party, '\\>)', collapse = '|')
    bls <- bls[grep(party, industry.title, ignore.case = T)]    
    write.csv(x = bls, file = paste0(save.path, year, '.csv'))
    rm(bls)
    gc()
    return(NULL)
}

years <- 2010:2016
parLapplyLB(cl, years, SubsetBLS)
