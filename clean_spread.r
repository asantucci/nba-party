################################################################################
################################################################################
################################################################################
###
### Title: Clean Spreads
###
### Andreas Santucci
###
### Date: February 2017
###
### Inputs: spreads data from goldensheet.com
###
################################################################################
################################################################################

require(data.table)
require(magrittr)
source('functions.r')

##############################
### Load Data
##############################

files <- list.files('raw_data', pattern = '[[:digit:]]{4}.txt', full.names = T)[-(1:3)]
data <- lapply(files, collectData)
data <- rbindlist(data)

##############################
### Clean Team Names
##############################

data[, team := as.character(team)]
data[, team := tolower(team)]
data[, opponent := tolower(opponent)]

data[opponent == 'okla. city', opponent := 'oklahoma city']
data[, opponent := gsub('^la ', 'los angeles ', opponent)]

data[team == 'okla. city thunder', team := 'oklahoma city thunder']
data[, team := gsub('^[[:blank:]]+', '', team)]

### Relationship between 'new jersey nets' and 'brooklyn nets', collapse into one?
  # #data[grepl("\\<nets$", team), team := 'brooklyn nets']

### Map team-abbreviations to proper names.
full.teams <- data[, unique(opponent)] %>%
    sapply(., grep, x = unique(data$team), value = T) %>%
    data.frame
full.teams[['abbr']] = rownames(full.teams)
setDT(full.teams)
setnames(full.teams, c('opponent.team', 'opponent'))
full.teams[, opponent.team := as.character(opponent.team)]

### Merge in propert opponent team names.
data <- merge(data, full.teams, by = 'opponent', all.x = T)
setnames(data, c('opponent', 'opponent.team'), c('opponent.team', 'opponent'))

##############################
### Spreads and Over-Unders
##############################

data <- data[spread != 'P'] # We lose 14 observations here...
data[, spread := gsub('\'$', '', spread) %>% as.numeric]

### Over-under.
data[, ou.out := gsub('^[0-9]+([OUN])$', '\\1', ou, ignore.case = T)]
data[, ou     := gsub('[OUN]$', '', ou, ignore.case = T) %>% as.numeric]

##############################
### Date Variable
##############################

### Carefully add in a year variable.
data[, year := NA_integer_]
data[grepl("^1[012]/",   date), year := substr(season, start = 1, stop = 4) %>% as.integer] # Oct-Dec
data[grepl("^[0-9]{1}/", date), year := substr(season, start = 6, stop = 10) %>% as.integer]# Jan-Sep

data[, date := paste(date, year, sep = '/') %>% as.Date(x = ., format = '%m/%d/%Y')]
data[, year := NULL]
data[, day  := weekdays(date)]

##############################
### Format Data
##############################

data[, opponent.team := NULL]
setcolorder(data, c('season', 'date', 'day', 'team', 'opponent', 'outcome', 'spread', 'score', 'location', 'ou', 'ou.out'))

##############################
### Lags
##############################

### Number of days since last game.
setkey(data, team, date)
data[, ndays.lgame := c(NA, diff(date)),    by = list(team, season)]
#data[, last.opp := c(NA, lag(opponent)[1:.N-1]), by = list(team, season)]

data[, game.loc := ifelse(location == 'H', team, opponent)]
data[, last.game.loc := c(NA, lag(game.loc)[1:.N-1]), by = list(team, season)]

save(data, file = 'tmp_data/spreads.RData')
