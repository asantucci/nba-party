################################################################################
################################################################################
################################################################################
###
### Title: Exploring Starters Data (from ESPN)
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
require(ggplot2)
require(lubridate)
require(magrittr)
require(sampling)
set.seed(4052017)

load(file = 'tmp_data/covers_lines.RData')
load(file = 'tmp_data/nmusician_estabs.RData')
load(file = 'tmp_data/population_by_county_year.RData')

musicians <- merge(musicians, pop, by = c('county', 'season'), all.x = T)
musicians[, county := gsub(' (county)|(division),.*$', '', county, ignore.case = T)]

lines <- merge(lines, musicians,
               by.x = c('season', 'last.game.loc'),
               by.y = c('season', 'team'), all.x = T)

setkey(lines, season, team, date)

HOURS <- 24
lines[, party := ifelse(nhours.lgame <= HOURS, nmusicians, 0)]

##################################################
### Total Points Allowed
##################################################

lines[, tpa := team.pts.admitted - mean(team.pts.admitted), by = list(team, season)]
lines[, tps := team.pts.scored   - mean(team.pts.scored), by = list(team, season)]

lm(tpa ~ party + I(hour(date))  + nhours.lgame, data = lines[nhours.lgame <= HOURS & last.game.loc != team]) %>% summary

##################################################
### ESPN
##################################################

espn <- fread('tmp_data/espn_player_data.csv')

### keeping starters makes a big difference in lag.chg.pos effect
#espn <- espn[espn[, .I[1:5], by = list(team, date)][, V1]]
espn[, date := as.Date(date)]
lines[, simple.date := as.character(date) %>% substr(., 1, 10) %>% as.Date]
data <- merge(lines, espn,
              by.x = c('team', 'season', 'simple.date'),
              by.y = c('team', 'season', 'date'), all.x = T)

### Estimate player fatigue, by looking at number of changes in posession.
data[, chg.pos := three.made + fg.made + free.attempted/2 + 
           to + (fg.attempted-fg.made + three.attempted - three.made - oreb)]
setkey(data, season, player, date)
data[, lag.chg.pos := shift(chg.pos), by = list(season, player)]

# Since we sum by location-date we double count.
changes <- data[, list(ttl.chg.pos = sum(chg.pos) / 2), 
            keyby = list(season, location, date)]
lines <- merge(lines, changes, by = c('season', 'location', 'date'))

setkey(lines, season, team, date)
lines[, lag.chg.pos := c(NA, lag(ttl.chg.pos)[1:.N-1]), by = list(season, team)]

### Good news: party significant after accounting for fatigue.
m <- glm(outcome == 'W' ~ party + lag.chg.pos + I(travel.dist*(travel.dist>1800)) + nhours.lgame +
             I(hour(date)),
         data = lines[season < 2017 & last.game.loc != team], family = 'binomial')

### SVM for out of the box performance.
lines <- lines[last.game.loc != team,
               list(hour = hour(date), last.game.time, travel.dist,
                    party, lag.chg.pos, season, outcome)] %>% na.omit

require(e1071)
m <- svm(x = lines[season < 2016, -c("outcome", "season"), with = F],
         y = lines[season < 2016, outcome == 'W'],
         scale = T, type = 'C-classification', probability = T, kernel = 'sigmoid')

preds <- predict(m, newdata = lines[season == 2016, -c("outcome", "season"), with = F])
table(preds, lines[season == 2016, outcome])

require(mboost)
lines[, game.time := hour(date)]
m <- gamboost(as.factor(outcome == 'W') ~ party + lag.chg.pos + travel.dist + nhours.lgame + last.game.time + game.time, data = lines[season < 2017], family = Binomial())
plot(m, which = "party", main = "Partial effect of Party on Meet Spread")
plot(m, which = "lag.chg.pos", main = "Partial effect of lagged change in possesons on Meet Spread")
plot(m, which = "travel.dist", main = "Partial effect of travel distance on Meet Spread") ## Gives intuition to add linear effect for travel.dist > 2k miles.
abline(v = 400)   # When teams get off the bus and start to fly?
abline(v = 1800)  # When jet-lag becomes too much and gets in the way of sleep?
plot(m, which = "last.game.time")
plot(m, which = "nhours.lgame")
preds <- predict(m, newdata = lines[season == 2017 & last.game.loc != team] %>% data.frame, type = 'response')
table(round(preds), lines[season == 2017 & last.game.loc != team, outcome])

### Defensive rebounds.
m <- gamboost(dreb.per.min ~ lag.mins + lag.chg.pos + party + nhours.lgame + last.game.time + travel.dist, data = data[season < 2017], family = Gaussian())
plot(m, which = "lag.mins") # It's not as though playing more minutes hurts you...
plot(m, which = 'lag.chg.pos', main = "The effect of lagged # changes in possesion for a player on rebound performance") # Too many chg. in possesions does hurt though...
abline(h = 0, col = 'red')

### Just making sure we can recover our old betting numbers.
HOURS <- 36
m <- glm(outcome == 'W' ~ I(nmusicians * (nhours.lgame < HOURS)),
         data = lines[season < 2017 & last.game.loc != team], family = 'binomial')

preds <- predict(m, newdata = lines[season == 2017 & last.game.loc != team & nhours.lgame < HOURS], type = 'response')
table(round(preds), lines[season == 2017 & last.game.loc != team & nhours.lgame < HOURS, outcome])
#cor(data[, grep('per', names(data)), with = F], data$lag.chg.pos, use = 'complete')

ggplot(data[nhours.lgame < 36],
       aes(jitter(lag.chg.pos), jitter(demeaned.dreb.per.min), color = party)) +
    geom_point()

ggplot(data, aes(jitter(lag.chg.pos), jitter(demeaned.pts.per.min))) +
    geom_point() +
    geom_smooth()

glm(outcome == 'W' ~ party + nhours.lgame + I(log(travel.dist+1)) + lag.chg.pos,
    data = lines[season < 2017 & last.game.loc != team],
    family = 'binomial') %>% summary

#data[, fatigue := ifelse(nhours.lgame < 36, lag.chg.pos, 0)]
lm(demeaned.reb.per.min ~ party + lag.chg.pos +
       I(log(travel.dist+1)/nhours.lgame) + nhours.lgame,
   data = data[season < 2017 & last.game.loc != team]) %>%
   summary

### WOW! Earn 1-2 points less per minute on average
lm(pm.per.min ~ party, data = data[season < 2017 & last.game.loc != team]) %>% summary

## data[grep("(los angeles)|(new york)|(brooklyn)", last.game.loc), 
##      list(average.d.dreb = mean(d.dreb)), by = player][order(average.d.dreb)] %>%
##     write.csv(., file = 'dmeaned.dreb.csv')

## data[grep("(los angeles)|(new york)|(brooklyn)", last.game.loc), 
##      list(average.d.pts = mean(d.pts)), by = player][order(average.d.pts)] %>%
##     write.csv(., file = 'dmeaned.pts.csv')


cur.cols <- grep("^((fg)|(three)|(free)|([od]?reb)|(pts)|(pm)|(demeaned)|(chg.pos)|(ast)|(blk)|(to)|(pf)|(stl)|(score)|(line)|(tps)|(tpa)|(team\\.pts)|(nmusicians)|(population)|(roll.demeaned.pm.per.min)|(roll.demeaned.pts.per.min))",
                 names(data), value = T)
data[, (cur.cols) := NULL]

m <- rpart(outcome == 'W' ~ ., data = data)
