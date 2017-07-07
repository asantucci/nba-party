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
Sys.setlocale('LC_ALL','C')

### Don't forget Washington D.C., which is technically a metro area.
pop <- fread(paste0('https://www2.census.gov/programs-surveys/popest/datasets',
                    '/2010-2016/metro/totals/csa-est2016-alldata.csv'),
            select = c('NAME', paste0('POPESTIMATE', 2010:2016)))
setnames(pop, c('area.title', paste0('popestimate', 2010:2016)))

### Reshape our data.
pop <- melt(pop, id.vars = 'area.title', variable.name = 'year')
pop[, year := gsub('popestimate', '', year) %>% as.integer]
setnames(pop, 'value', 'population')


### Notice that Washington DC appears twice.
### We check 'https://www.census.gov/quickfacts/fact/table/DC#viewtop' for another
### estimate, and take the more consistent observation.
### E.g.: pop[area.title == 'Washington-Arlington-Alexandria, DC-VA-MD-WV'] %>% unique
pop <- pop[, .SD[which.max(population)], by = .(area.title, year)]

### For some reason, San Antonio-New Braunfels metro-area not included in data-file above.
dt <- fread(paste0('https://www2.census.gov/programs-surveys/popest/datasets',
                   '/2010-2016/metro/totals/cbsa-est2016-alldata.csv'),
            select = c('NAME', paste0('POPESTIMATE', 2010:2016)))
setnames(dt, c('area.title', paste0('popestimate', 2010:2016)))
dt <- melt(dt, id.vars = 'area.title', variable.name = 'year')
dt[, year := gsub('popestimate', '', year) %>% as.integer]
setnames(dt, 'value', 'population')
dt <- dt[grepl("(phoenix)|(san antonio)", area.title, ignore.case = T)]

pop <- rbind(pop, dt)
save(pop, file = 'tmp_data/population_by_MSA_year.RData')
