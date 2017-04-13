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
espn[, date := as.Date(date)]
lines[, simple.date := as.character(date) %>% substr(., 1, 10) %>% as.Date]
data <- merge(lines, espn,
              by.x = c('team', 'simple.date'),
              by.y = c('team', 'date'), all.x = T)

### Create player-season demeaned features.
data[, d.free  := free.made/min - mean(free.made/min), by = list(starters, season)]
data[, d.three := three.made/min - mean(three.made/min), by = list(starters, season)]
data[, d.reb   := reb  / min - mean(reb  / min), by = list(starters, season)]
data[, d.oreb  := oreb / min - mean(oreb / min), by = list(starters, season)]
data[, d.dreb  := dreb / min - mean(dreb / min), by = list(starters, season)]
data[, d.pts   := pts  / min - mean(pts  / min, na.rm=T), by = list(starters, season)]

### Estimate player fatigue, by looking at number of changes in posession.
data[, chg.pos := three.made + fg.made + free.attempted/2 + 
           to + (fg.attempted-fg.made + three.attempted - three.made - oreb)]
changes <- data[, list(ttl.chg.pos = sum(chg.pos)), 
            keyby = list(season, team, date, opponent)]
lines <- merge(lines, changes, by = c('season', 'team', 'date', 'opponent'))

setkey(lines, season, team, date)
lines[, lag.chg.pos := c(NA, lag(ttl.chg.pos)[1:.N-1]), by = list(season, team)]

glm(outcome == 'W' ~ party + nhours.lgame + I(log(travel.dist+1)) + lag.chg.pos,
    data = lines[season < 2017 & last.game.loc != team],
    family = 'binomial') %>% summary

glm(outcome == 'W' ~ lag.chg.pos,
    data = lines[season < 2017 &
                 lag.chg.pos < quantile(lag.chg.pos, 0.98,na.rm=T)],
    family = 'binomial') %>%
    summary

require(ggplot2)


ggplot(data = lines, aes(x = lag.chg.pos, y = jitter((outcome == 'W') %>% as.numeric))) +
    geom_point()

setnames(data, '+/-', 'pm')

lm(d.three ~ party, data = data[season < 2017 & last.game.loc != team]) %>% summary
lm(d.reb ~ party, data = data[season < 2017 & last.game.loc != team]) %>% summary
lm(d.dreb ~ party, data = data[season < 2017 & last.game.loc != team]) %>% summary

data[, pm := as.numeric(pm)]
data[, pf := as.numeric(pf)]

### WOW! Earn 1-2 points less per minute on average
lm(I(pm / min) ~ party, data = data[season < 2017 & last.game.loc != team]) %>% summary

lm(d.pts ~ party, data = data[season < 2017 & last.game.loc != team]%>%na.omit) %>% summary

## data[grep("(los angeles)|(new york)|(brooklyn)", last.game.loc), 
##      list(average.d.dreb = mean(d.dreb)), by = starters][order(average.d.dreb)] %>%
##     write.csv(., file = 'dmeaned.dreb.csv')

## data[grep("(los angeles)|(new york)|(brooklyn)", last.game.loc), 
##      list(average.d.pts = mean(d.pts)), by = starters][order(average.d.pts)] %>%
##     write.csv(., file = 'dmeaned.pts.csv')

