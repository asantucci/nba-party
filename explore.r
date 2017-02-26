
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
data[, spread := gsub('\'$', '', spread) %>% as.numeric]

data[, date := paste(date, year, sep = '/') %>% as.Date(x = ., format = '%m/%d/%Y')]
data[, year := NULL]
data[, day  := weekdays(date)]

### Another 24 obs lost here.
data <- data[!is.na(date)]

### Number of days since last game.
setkey(data, team, date)
data[, last.game := c(NA, diff(date)), by = list(team, year(date))]
data[, last.opp  := c(NA, lag(opponent), by = list(team, year(date)))]
data[, last.loc  := c(NA, lag(location), by = list(team, year(date)))]

##############################
## What does it mean to party?
##############################

cities <- c("miami", "los angeles", "^la\\>", "^nets$", "brooklyn", "new york", "atlanta")
rgx <- paste0('(', cities, ')', collapse = "|")

### Generate a lag-variable which describes the location of the most-recent game played.
setkey(data, team, date)

data[, party := 0]

### We (1) exclude people who already party, (2) check if last game played against party city
### (3) check last game was played 'away' i.e. at party city and (4) party last night
data[!grepl(rgx, team) & grepl(rgx, last.opp) & last.loc == 'V' & last.game == 1, party := 1]

### We also consider: when the team had the day off the day before playing a party city
data[grepl('V', location) & grepl(rgx, opponent) & last.game > 1 & !grepl(rgx, team), table(outcome)]

### How often are teams meeting the spread?
hist(data[, mean(outcome == 'W'), by = list(year(date), team)][, V1], breaks = seq(0.3, 0.8, by = 0.02), main = "How often are teams meeting the spread?", xlab = "Proportion of games for which a team-year meets the spread")

table(data$party, data$outcome)  # Only win 44% of the games.
table(data[last.game == 1, outcome]) # Win 49% of the games they play the day before.
