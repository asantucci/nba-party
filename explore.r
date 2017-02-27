
## require(RSelenium)
## # https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
## # andreas@asantucci-MBP:~/Downloads$ java -jar selenium-server-standalone-2.53.1.jar 
## remDr <- remoteDriver(port = 4444)
## remDr$open()
## remDr$navigate('http://www.goldsheet.com/histnba.php')

require(data.table)
require(magrittr)
source('functions.r')

files <- list.files('raw_data', pattern = '[[:digit:]]{4}.txt', full.names = T)
data <- lapply(files, collectData)
data <- rbindlist(data)

data[, team := tolower(team)]
data[, opponent := tolower(opponent)]

data[team == 'okla. city thunder', team := 'oklahoma city thunder']
data[, team := gsub('^[[:blank:]]+', '', team)]

### Relationship between 'new jersey nets' and 'brooklyn nets', collapse into one?
data[grepl("\\<nets$", team), team := 'nets']

### We lose 16 observations here...
data <- data[spread != 'P']
data[, spread := gsub('\'$', '', spread) %>% as.numeric]

data[, year := NA_integer_]
data[grepl("^1[012]/", date), year := substr(season, start = 1, stop = 4) %>% as.integer]
data[grepl("^[0-9]{1}/", date), year := substr(season, start = 6, stop = 10) %>% as.integer]

data[, date := paste(date, year, sep = '/') %>% as.Date(x = ., format = '%m/%d/%Y')]
data[, year := NULL]
data[, day  := weekdays(date)]

### Number of days since last game.
setkey(data, team, date)
data[, last.game := c(NA, diff(date)),    by = list(team, season)]
data[, last.opp  := c(NA, lag(opponent)[1:.N-1]), by = list(team, season)]
data[, last.loc  := c(NA, lag(location)[1:.N-1]), by = list(team, season)]

##############################
## What does it mean to party?
##############################

party.cities <- c("miami", "los angeles", "^la\\>", "^nets$", "brooklyn", "new york", "atlanta")
party.rgx <- paste0('(', party.cities, ')', collapse = "|")

### Generate a lag-variable which describes the location of the most-recent game played.
setkey(data, team, date)

### 2009-2010
bad.2009 <- data.frame(season = '2009_2010',
                       bad.teams = paste0('(', c('milwaukee', 'toronto', 'new york',
                                                 'washington', 'memphis', 'minnesota',
                                                 'oklahoma city', 'clippers', 'sacramento'),
                                          ')', collapse = '|'))

### 2010-2011
bad.2010 <- data.frame(season = '2010_2011',
                       bad.teams = paste0('(', c('cleveland', 'detroit', 'philadelphia',
                                                 'washington', '(new jersey)|(\\<nets)',
                                                 'clippers', 'golden state', 'sacramento',
                                                 'minnesota'),
                                          ')', collapse = '|'))

### 2011-2012
bad.2011 <- data.frame(season = '2011_2012',
                       bad.teams = paste0('(', c('\\<nets', 'washington', 'toronto',
                                                 'cleveland', 'golden state', 'sacramento' ,
                                                 'minnesota'),
                                          ')', collapse = '|'))

### 2012-2013
bad.2012 <- data.frame(season = '2012_2013',
                       bad.teams = paste0('(', c('orlando', 'philadelphia', 'cleveland',
                                                 'washington', 'charlotte', 'minnesota',
                                                 'golden state', 'sacramento', 'new orleans'),
                                          ')', collapse = '|'))

#bad.teams <- paste0('(', bad.teams, ')', collapse = '|')

### To do:
### 1. is the team a playoff contender?
### 2. Fix the way year is assigned to date variable.

data[, bad.team := 0L]
data[season == '2009_2010' & grepl(bad.2009$bad.teams, team), bad.team := 1]
data[season == '2010_2011' & grepl(bad.2010$bad.teams, team), bad.team := 1]
data[season == '2011_2012' & grepl(bad.2011$bad.teams, team), bad.team := 1]
data[season == '2012_2013' & grepl(bad.2012$bad.teams, team), bad.team := 1]

data[, party := 0]

### We (1) exclude people who already party, (2) check if last game played against party city
### (3) check last game was played 'away' i.e. at party city and (4) party last night
data[!grepl(party.rgx, team) & grepl(party.rgx, last.opp) & last.loc == 'V' & last.game == 1, party := 1]
## data[grepl(party.rgx, last.opp) & last.loc == 'V' & last.game == 1 & bad.team == 1, party := 1]

### We also consider: when the team had the day off the day before playing a party city
#data[grepl('V', location) & grepl(party.rgx, opponent) & last.game > 1, table(outcome)]

### How often are teams meeting the spread?
hist(data[, mean(outcome == 'W'), by = list(season, team)][, V1],
     breaks = seq(0.3, 0.75, by = 0.025),
     main = "How often are teams meeting the spread?",
     xlab = "Proportion of games for which a team-year meets the spread")

table(data$party, data$outcome)      # Only win 44% of the games.
table(data[last.game == 1, outcome]) # Win 49% of the games they play the day before.

model <- glm(outcome=='W' ~ last.game + party, family = 'binomial', data = data)

### logistic
### probability of meeting the spread ~ # days since last game + # games played in last week +
###  + day of week + team-ranking + party city

### proportion of games where spread is met if spread is over abs(X)
