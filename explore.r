
## require(RSelenium)
## # https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
## # andreas@asantucci-MBP:~/Downloads$ java -jar selenium-server-standalone-2.53.1.jar 
## remDr <- remoteDriver(port = 4444)
## remDr$open()
## remDr$navigate('http://www.goldsheet.com/histnba.php')

### To do:
### 1. is the team a playoff contender?
### 2. Fix the way year is assigned to date variable.

require(data.table)
require(ggmap)
require(magrittr)
require(sp)
source('functions.r')

load(file = 'tmp_data/spreads_wdist.RData')
load(file = 'tmp_data/standings.RData')

data[, season := gsub('_20', '_', season)]

standings <- subset(standings, select = c('team', 'season', 'pct'))

data <- merge(data, standings, by = c('season', 'team'), all.x = T)

##############################
## What does it mean to party?
##############################

party.cities <- c("miami", "los angeles", "brooklyn", "new york", "atlanta")
party.rgx <- paste0('(', party.cities, ')', collapse = "|")

data[, party := 0]
data[, weekend := grepl("(Friday)|(Saturday)|(Sunday)", day) %>% as.integer]

### We (1) exclude people who already party, (2) check if last game played against party city
### (3) check last game was played 'away' i.e. at party city and (4) party last night
data[!grepl(party.rgx, team) & grepl(party.rgx, last.game.loc) & ndays.lgame == 1, party := 1]
## data[grepl(party.rgx, last.opp) & last.loc == 'V' & last.game == 1 & bad.team == 1, party := 1]

### We also consider: when the team had the day off the day before playing a party city
#data[grepl('V', location) & grepl(party.rgx, opponent) & last.game > 1, table(outcome)]

### How often are teams meeting the spread?
hist(data[, mean(outcome == 'W'), by = list(season, team)][, V1],
     breaks = seq(0.3, 0.75, by = 0.025),
     main = "How often are teams meeting the spread?",
     xlab = "Proportion of games for which a team-year meets the spread")

table(data$party, data$outcome)      # Only win 44% of the games.
table(data[ndays.lgame == 1, outcome]) # Win 49% of the games they play the day before.

data[, pq := cut(pct, breaks = quantile(pct)), by = season]
data[, hp := ifelse(pq == '(0.244,0.378]' & party == 1, 1, 0)]

model <- glm(outcome=='W' ~ party + weekend + ndays.lgame + pct + I(travel.dist>1350), family = 'binomial', data = data)

### logistic
### probability of meeting the spread ~ # days since last game + # games played in last week +
###  + day of week + team-ranking + party city

### proportion of games where spread is met if spread is over abs(X)
