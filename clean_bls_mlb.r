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

source('functions.r')

# 'Audio and video media reproduction', 'Arts, entertainment, and recreation'
party <- c('sound recording', 'music publisher', 'musical group')

years <- 2010:2016
parLapplyLB(cl, years, SubsetBLS, party.regex = party, sport = 'mlb')

files <- list.files(path = 'tmp_data/bls', pattern = 'mlb\\.csv$', full.names = T)
bls <- lapply(files, fread) %>% rbindlist

### Load covers data to get a listing of teams.
abbrs <- fread(file = 'mlb_abbrs.csv')
setnames(abbrs, c('abbr', 'full', 'nick'))

### Geocode the locations for each of our MLB teams.
locs  <- lapply(abbrs$full, geocode, output = 'more') %>% rbindlist(., fill = T)
setnames(locs, gsub('_', '.', names(locs)))
locs <- locs[, lapply(.SD, as.character), .SDcols = 1:ncol(locs)]

### Be careful, since Washington Wizards are in Washington DC (so not a county)
locs[, county := ifelse(is.na(administrative.area.level.2),
                        administrative.area.level.1,
                        paste(administrative.area.level.2,
                              administrative.area.level.1, sep = ', '))]
locs <- data.table(team = abbrs$full, county = locs$county)
locs[county == 'Maryland', county := 'Baltimore County, Maryland']
locs[county == 'Missouri', county := 'Clay County, Missouri']
locs[team == 'atlanta braves', county := 'Fulton County, Georgia'] # Changes in 2017.
save(locs, file = 'tmp_data/mlb_team_locations.RData')

musicians <- merge(locs, bls, by.x = 'county', by.y = 'area.title', all.x = T)
musicians <- musicians[, list(nmusicians = mean(qtrly.estabs.count %>% as.numeric)),
                       by = list(county, team, year)][order(year, nmusicians)]

### We'll use lagged data (sentiment about which city has the most bars/drinking
  #  establshmnets may evolve slowly, so this is not unreasonable)
setnames(musicians, 'year', 'season')
musicians[, season := season %>% as.numeric]

save(musicians, file = 'tmp_data/nmusician_estabs_mlb.RData')

## load('tmp_data/population_by_county_year.RData')
## musicians <- merge(pop, musicians)
## save(musicians, file = 'tmp_data/nmusician_estabs_mlb.RData')
