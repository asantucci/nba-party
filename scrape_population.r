################################################################################
################################################################################
################################################################################
###
### Title: Scraping Population Data
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

require(data.table)
require(magrittr)
require(RCurl)
require(XML)

### Download population estimates by county-year.
pop <- fread('https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/counties/totals/co-est2016-alldata.csv',
             select = c('STNAME', 'CTYNAME', paste0('POPESTIMATE', 2010:2016)))
setnames(pop, tolower(names(pop)))

### Don't forget Washington D.C., which is technically a metro area.
dc <- fread('https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/metro/totals/csa-est2016-alldata.csv',
            select = c('NAME', paste0('POPESTIMATE', 2010:2016)))
Sys.setlocale('LC_ALL','C') 
dc <- dc[grep("district", NAME, ignore.case = T)]
dc[, stname := 'Washington DC']
setnames(dc, c('ctyname', paste0('popestimate', 2010:2016), 'stname'))
dc[, ctyname := 'District of Columbia']
pop <- rbind(pop, dc)

### Reshape our data.
pop <- melt(pop, id.vars = c('stname', 'ctyname'), variable.name = 'year')
pop[, year := gsub('popestimate', '', year) %>% as.integer]
setnames(pop, 'value', 'population')

setnames(pop, c('stname', 'ctyname'), c('state', 'county'))

### We use lagged data.
setnames(pop, 'year', 'season')
pop[, season := season + 1]
pop <- pop[state != county]
pop[, county := paste(county, state, sep = ', ')]
pop[, state := NULL]
pop[county == 'District of Columbia, Washington DC', county := 'District of Columbia']

save(pop, file = 'tmp_data/population_by_county_year.RData')

### Old
## fips <- readLines('https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt')
## fips <- strsplit(fips, split = ',', fixed = T) %>% do.call(rbind, .) %>% data.table
## fips[, ncol(fips) := NULL, with = F]
## setnames(fips, c('state', 'state.fips', 'county.fips', 'county'))
