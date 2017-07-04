################################################################################
################################################################################
################################################################################
###
### Title: Exploring Covers Data
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
require(ggmap)
require(sp)

load('tmp_data/covers_lines.RData')

##################################################
### Exploring data
##################################################

require(ggplot2)

hist(lines[, mean(outcome == 'W'), by = list(season, team)][, V1],
     breaks = seq(0.3, 0.75, by = 0.025),
     main = "How often are teams meeting the spread?",
     xlab = "Proportion of games for which a team-year meets the spread")

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N),
             by = list(last.game.loc, ndays.lgame)][order(ndays.lgame)]

ggplot(tmp %>% na.omit, aes(x = ndays.lgame, y = meet.spread, label = N)) +
    geom_point() +
    geom_label() + 
    geom_hline(yintercept = 0.5) + 
    facet_wrap(facets = ~ last.game.loc)

lines[party==1, .N, by = list(season, team)][, N] %>%
    hist(main = "Number of party days by season-team", xlab = "Number of days",
         ylab = "Frequency")

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N), 
            by = list(pct, party)][order(pct)]
ggplot(tmp, aes(x = pct, y = meet.spread, color = as.factor(party), label = N)) + 
    geom_label() +
    geom_hline(yintercept = 0.5) + 
    geom_smooth()

m <- lines[,#grepl(party.rgx, game.loc) & !grepl(party.rgx, team),
          list(meet.spread = mean(outcome == 'W'), pct = unique(pct), .N), by = list(team, season)]
ggplot(m, aes(x = pct, y = meet.spread)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(facets = ~season) + 
    labs(title = "In select seasons, lagged performance not predictive of meet the spread",
         x = "Percentage of games won last season",
         y = "Proportion of games for which spread was met in current season")

m <- lines[, list(meet.spread = mean(outcome == 'W'), .N), by = ndays.lgame][order(ndays.lgame)]
ggplot(m, aes(x = ndays.lgame, y = meet.spread, label = N)) +
    geom_point() +
    geom_label(nudge_y = -.04) +
    geom_hline(yintercept = 0.5) +
    scale_x_continuous(limits = c(1, 12), breaks = 1:12, labels = 1:12) +
    labs(x = "Number of days since last game", y = "Met the Spread",
         title = paste("Proportion of games which meet spread",
                       "as a function of rest-time",
                       "(Counts indicate number of observations used)", sep = "\n"))

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N), by = list(team, party)][order(team, party)]

hist(lines$avg.age)

tmp <- lines[, list(meet.spread = mean(outcome == 'W'), .N), 
            by = list(avg.age = round(avg.age, 2), party)][
    order(avg.age, party)] %>% na.omit

ggplot(tmp, aes(x = avg.age, y = meet.spread, 
                color = party %>% as.factor, label = N)) + 
    geom_point() +
    geom_label() +
    geom_smooth()

##################################################
### Toy model.
##################################################

model <- glm(outcome == 'W' ~ party + ndays.lgame + I(log(travel.dist+1)) + season,
             data = lines, family = 'binomial', subset = season < 2016)

### Team effects are not jointly significant.
## model2 <- glm(outcome == 'W' ~ party + avg.age + party:avg.age + ndays.lgame + team + 
##                  pct + weekend + I(log(travel.dist+1)) + as.factor(season),
##               data = lines, family = 'binomial')

## var.test(model, model2)

summary(model)

yhat <- predict(model, newdata = lines[season >= 2016 & party == 1], type = 'response')
table(yhat < 0.41, lines[season >= 2016 & party == 1, outcome])

## lines[season == 2017 & party == 1, table(outcome)]
## lines[season == 2017 & party == 1 & grepl('sixer', team), table(outcome)]
## lines[season == 2017 & party == 1 & grepl('chicago', team), table(outcome)]
## lines[season == 2017 & party == 1 & grepl('phoenix', team), table(outcome)]
## lines[season == 2017 & party == 1 & grepl('sacramento', team), table(outcome)]
## lines[season == 2017 & party == 1 & grepl('(orlando)|(philadelphia)|(phoenix)|(sacramento)|(minnesota)', team), table(outcome)]
