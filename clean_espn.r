################################################################################
################################################################################
################################################################################
###
### Title: Cleaning ESPN Player Data
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

require(parallel)
cl <- makeCluster(detectCores())

### Set up abbreviations to expand.
abbrs <- fread('tmp_data/team_abbreviations.csv')
abbrs[team == 'brooklyn nets', abbr := 'bkn']
abbrs[team == 'washington wizards', abbr := 'wsh']
abbrs[team == 'utah jazz', abbr := 'utah']
abbrs[team == 'phoenix suns', abbr := 'phx']

abbrs <- rbind(abbrs, data.frame(team = 'brooklyn nets', abbr = 'nj'))

### Load up ESPN raw data.
files <- list.files('raw_data/espn', full.names = T)
data <- parLapplyLB(cl, files, fread) %>% rbindlist(., fill = T)
setnames(data, c('player', 'min', 'fg', 'three.pt', 'ft',
                 'oreb', 'dreb', 'reb', 'ast', 'stl', 'blk',
                 'to', 'pf', 'pm', 'pts', 'team', 'date'))
data[, team := tolower(team)]
#data <- na.omit(data)

data <- merge(data, abbrs, by.x = 'team', by.y = 'abbr', all.x = T)
data[, 1 := NULL, with = F]

data <- data[!grepl("^-+$", fg)]

### Remove duplicated names.
data[, player := gsub('(.*)\\1', '\\1', player)]
data[, c('fg.made', 'fg.attempted') :=       tstrsplit(fg,       split = '-', fixed=T)]
data[, c('three.made', 'three.attempted') := tstrsplit(three.pt, split = '-', fixed=T)]
data[, c('free.made', 'free.attempted') :=   tstrsplit(ft,       split = '-', fixed=T)]

data[, date := as.Date(date)]
data <- data[!is.na(date)]
data[, season := ifelse(month(date) %in% 10:12, year(date) + 1, year(date))]

### Translate numeric variables to their appropriate type.
data[, c('fg', 'three.pt', 'ft') := NULL]
cols <- CJ(c('fg', 'three', 'free'), c('made', 'attempted'))
cols <- paste(cols[[1]], cols[[2]], sep = '.')
cols <- c(cols, 'min', 'reb', 'oreb', 'dreb', 'pts', 'to', 'pm', 'pf')
data <- data[!grepl("[^0-9+-]", min) & min != '0']

for (col in cols)
    set(data, j = col, value = as.numeric(data[[col]]))

### Demean by season-player
cols <- Filter(function(x) !(x %in% c('min', 'to','pf')), cols)
setnames(data, 'min', 'mins')
### (First, we scale by minutes played...)
for (c in cols) data[, (paste0(c, '.per.min')) := get(c) / mins]
cols <- paste0(cols, '.per.min')
### (...then, we demean)
data[, (cols) := lapply(.SD, function(x) x - cumsum(x)/.I),
     by = list(season, player), .SDcols = cols]

data[, position := gregexpr(pattern = '[A-Z]{1,2}$', player) %>%
                   regmatches(x = player) %>% unlist]

data[, player := gsub('[A-Z]{1,2}$', '', player)]

### Lagged variables.
setkey(data, season, player, date)
cols <- c(cols, 'mins')
data[, (paste0('lag.', cols)) := shift(.SD),
     by = list(season, player), .SDcols = cols]

write.csv(data, file = 'tmp_data/espn_player_data.csv', row.names = F)
