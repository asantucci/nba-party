require(ggmap)
require(magrittr)

files <- list.files(path = 'tmp_data/bls')
lapply(files, 
load(file = 'tmp_data/bls.RData')
load(file = 'tmp_data/covers_lines.RData')
lines[, `:=`(pct = NULL, weekend = NULL)]
others <- grep("party", names(lines), value = T, invert = T)
setcolorder(lines, c(others, c('party')))

teams <- unique(lines$team)
locs  <- lapply(teams, geocode, output = 'more') %>% rbindlist(., fill = T)
setnames(locs, gsub('_', '.', names(locs)))
locs <- locs[, lapply(.SD, as.character), .SDcols = 1:ncol(locs)]
locs[, county := ifelse(is.na(administrative.area.level.2),
                        administrative.area.level.1,
                        paste(administrative.area.level.2,
                              administrative.area.level.1, sep = ', '))]
locs <- data.table(team = teams, county = locs$county)
save(locs, file = 'team_locations.RData')

locs <- merge(locs, bls, by.x = 'county', by.y = 'area.title', all.x = T)

locs <- locs[, list(ndrinking.estabs = mean(qtrly.estabs.count %>% as.numeric)),
             by = list(county, team, year)][order(ndrinking.estabs)]

### Used to check that each team matches exactly one county in BLS data
### (Except for Toronto Raptors)
## for (t in locs$county)
##     cat(paste0("Team ", t, " matched with ",
##                bls[grep(t, area.title, ignore.case = T), length(unique(area.title))], '\n'))
