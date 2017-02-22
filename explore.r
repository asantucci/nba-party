
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

data[, date := paste(date, year, sep = '/') %>% as.Date(x = ., format = '%m/%d/%Y')]
data[, year := NULL]

### We lose 16 observations here...
data[, spread := gsub('\'$', '', spread) %>% as.numeric]

### Define what it means to party.
cities <- c("miami", "los angeles", "^la\\>", "^nets$", "brooklyn", "new york")
rgx <- paste0('(', cities, ')', collapse = "|")

data[, party := 0]
data[grepl(rgx, team) & location == 'H', party := 1]
data[grepl(rgx, opponent) & location == 'V', party := 1]

