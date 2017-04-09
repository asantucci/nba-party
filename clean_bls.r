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

### Notes and data-sources.
  # http://www.nciaa.com/content.aspx?page_id=22&club_id=160641&module_id=29898

##################################################
### Set up Workspace
##################################################

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

##############################
### Subset BLS Data into manageable chunks.
##############################

party <- c('sound recording', 'music publisher', 'musical group')

SubsetBLS <- function(year, party.regex,
                      raw.path = 'raw_data/bls/', save.path = 'tmp_data/bls/') {
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
    party.regex <- paste0('(', party.regex, ')', collapse = '|')
    bls <- bls[grep(party.regex, industry.title, ignore.case = T)]    
    write.csv(x = bls, file = paste0(save.path, year, '.csv'), row.names = F)
    rm(bls)
    gc()
    return(NULL)
}

years <- 2010:2016
parLapplyLB(cl, years, SubsetBLS, party.regex = party)

##################################################
### Merge in BLS data with corresponding NBA teams.
##################################################

### Load BLS data.
files <- list.files(path = 'tmp_data/bls', pattern = 'csv$', full.names = T)
bls <- lapply(files, fread) %>% rbindlist

### Load Covers data (simply to get a listing of team-names, it's a bit over-kill)
load(file = 'tmp_data/covers_lines.RData')
lines[, `:=`(pct = NULL, weekend = NULL)]
others <- grep("party", names(lines), value = T, invert = T)
setcolorder(lines, c(others, c('party')))

### Geocode the locations for each of our NBA teams.
teams <- unique(lines$team)
locs  <- lapply(teams, geocode, output = 'more') %>% rbindlist(., fill = T)
setnames(locs, gsub('_', '.', names(locs)))
locs <- locs[, lapply(.SD, as.character), .SDcols = 1:ncol(locs)]

### Be careful, since Washington Wizards are in Washington DC (so not a county)
locs[, county := ifelse(is.na(administrative.area.level.2),
                        administrative.area.level.1,
                        paste(administrative.area.level.2,
                              administrative.area.level.1, sep = ', '))]
locs <- data.table(team = teams, county = locs$county)
save(locs, file = 'tmp_data/team_locations.RData')

musicians <- merge(locs, bls, by.x = 'county', by.y = 'area.title', all.x = T)
musicians <- musicians[, list(nmusicians = mean(qtrly.estabs.count %>% as.numeric)),
                       by = list(county, team, year)][order(year, nmusicians)]

### We'll use lagged data (sentiment about which city has the most bars/drinking
  #  establshmnets may evolve slowly, so this is not unreasonable)
setnames(musicians, 'year', 'season')
musicians[, season := season %>% as.numeric %>% `+`(1)] 
save(musicians, file = 'tmp_data/nmusician_estabs.RData')

### Used to check that each team matches exactly one county in BLS data
  # (Except for Toronto Raptors)
## for (t in locs$county)
##     cat(paste0("Team ", t, " matched with ",
##                bls[grep(t, area.title, ignore.case = T), length(unique(area.title))], '\n'))

