################################################################################
################################################################################
################################################################################
###
### Title: Model
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: 'tmp_data/spreads_with_standings.RData'
###
################################################################################
################################################################################

require(data.table)
require(magrittr)
source('functions.r')

load(file = 'tmp_data/spreads_with_standings.RData')

table(data$party, data$outcome)      # Only win 44% of the games.
table(data[ndays.lgame == 1, outcome]) # Win 49% of the games they play the day before.

data[, pq := cut(pct, breaks = quantile(pct)), by = season]
data[, hp := ifelse(pq == '(0.244,0.378]' & party == 1, 1, 0)]

party.teams <- c('charlotte bobcats',   'chicago bulls',      'denver nuggets',
                 'new orleans hornets', 'philadelphia 76ers', 'houston rockets')

model <- glm(outcome=='W' ~ party + weekend + ndays.lgame + pct + I(travel.dist>1350),
             family = 'binomial', data = data, subset = season != '2008_09' & team %in% party.teams)

model <- glm(outcome == 'W' ~ party + weekend + ndays.lgame + pct +
                 travel.dist + team + season + team:season,
             family = 'binomial', data = data, subset = season != '2008_09')

### logistic
### probability of meeting the spread ~ # days since last game + # games played in last week +
###  + day of week + team-ranking + party city

### proportion of games where spread is met if spread is over abs(X)
