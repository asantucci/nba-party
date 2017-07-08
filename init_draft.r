################################################################################
################################################################################
################################################################################
###
### Title: Andreas Santucci
###
### Andreas Santucci
###
### Date: July 2017
###
### Inputs: 'tmp_data/nba.RData'
###         'tmp_data/mlb.RData'
###
### Dependencies: data.table, magrittr, stargazer
###
################################################################################
################################################################################

##################################################
### Set up workspace.
##################################################

require(data.table)
require(magrittr)
require(stargazer)

load(file = 'tmp_data/nba.RData')  # nba.lines, espn.
load(file = 'tmp_data/mlb.RData')  # mlb.lines

##################################################
### NBA Modeling.
##################################################

### Discrete indicator.
m0 <- glm(outcome == 'W' ~ party.discrete + lag.chg.pos + log(travel.dist+1)*ew + 
             nhours.lgame + I(hour(date)) + I(team==location) + as.factor(season),
         data = nba.lines[season < 2017 & !is.na(party)], family = 'binomial',
         na.action = 'na.exclude')

### Continuous measure.
m1 <- update(m0, . ~ party + . - party.discrete)

### Placebo model.
mp <- update(m0, . ~ party.placebo + . - party.discrete)

### Player specific model - demeaned rebounds per minute.
## m2 <- lm(demeaned.reb.per.min ~ party + lag.chg.pos + log(travel.dist+1) + nhours.lgame +
##              as.factor(season),
##    data = espn[last.game.loc != team] %>% na.omit)

### Total points admitted and scored.
tpa <- lm(team.pts.admitted ~ party + lag.chg.pos + log(travel.dist+1) + nhours.lgame,
   data = espn[last.game.loc != team] %>% na.omit)
tps <- update(tpa, team.pts.scored ~ .)

### Points  allowed as afunction of last game location (by party)
nba.lines[, demeaned.pts.admitted := team.pts.admitted - mean(team.pts.admitted),
          by = list(season, team)]
mpa1 <- lm(demeaned.pts.admitted ~ party.discrete, data = nba.lines)

##################################################
### MLB Modeling
##################################################

m3 <- glm(I(team.score > opponent.score) ~ party + odds + I(team == location) + 
             ndays.lgame + log(travel.dist+1) + weekend + as.factor(season),
         data = mlb.lines[season < 2017], family = 'binomial')
summary(m3)
m4 <- update(m3, . ~ party.now + . - party)

##################################################
### MLB Betting.
##################################################
pdf('writing/mlb_bets_by_season.pdf')
par(mfrow = c(3,2))
b <- lapply(2012:2016, function(yr) {
    m <- glm(I(team.score > opponent.score) ~ party + odds,
             data = mlb.lines[season < yr], family = 'binomial')
    p <- predict(m, newdata = mlb.lines[year(date) == yr], type = 'response')
    ### Subset data to relevant year we wish to predict on; make predictions.
    tmp <- mlb.lines[season == yr, .(matchup, date, team, opponent, 
                                     team.score, opponent.score, odds)]
    tmp[, fitted := predict(m, newdata = mlb.lines[season == yr], type = 'response')]
    ### Realize that we can only make one prediction per game. So take the stronger bet.
    setkey(tmp, date, matchup)
    tmp <- tmp[, .SD[which.max(abs(fitted - .5))], by = list(matchup, date)]
    tmp <- mlb.lines[tmp, on = intersect(names(tmp), names(mlb.lines))]
    ### Translate fitted probabilities into 0-1 predictions if our odds differ by threshold amount.
    tmp[, prediction := mapply(hangover::Predict, prediction = tmp$fitted, vegas = tmp$odds, 
                               MoreArgs = list(threshold = 0.02))] # 0.02 was chosen ad-hoc.
    tmp <- tmp[!is.na(prediction)]
    ### Translate prediction into a monetary gain or loss based on odds and actual outcome.
    tmp[, bet := mapply(hangover::Bet, our.prediction = prediction, 
                        actual.outcome = team.score > opponent.score,
                        odds = odds)]
    ### Plot the cumulative sum of profits over the course of the season.
    tmp[, running.prof := cumsum(bet)]    
    cat(paste0(sum(tmp$bet), '\n'))
    plot(x = 1:nrow(tmp), y = tmp$running.prof, 
         main = paste0('Profit over the ', yr, ' season: $', 
                       format(round(sum(tmp$bet), digits = -1), big.mark = ','), '\n',
                       'Training on 2011-', yr-1, ' data'),
         xlab = 'Game Number', ylab = 'Profit', type = 'l')
    abline(a = 0, b = 0, lty = 3)
    points(x = which.min(tmp$running.prof), y = min(tmp$running.prof), type = 'p')
    text(x = which.min(tmp$running.prof), y = min(tmp$running.prof), 
         labels = round(min(tmp$running.prof)), pos = 4)
    return(tmp[, bet])
})
dev.off()

##################################################
### Stargazing.
##################################################
stargazer(m0, m1, covariate.labels = c('Party discrete', 'Party continuous', 
                                       'Lag changes in posession', 
                                       'Logged travel distance', 
                                       'East-west travel direction', 
                                       'Number hours rest time', 
                                       'Time of game during day', 
                                       'Home team effect', 
                                       'Logged travel distance * east-west', 
                                       'Constant'),
          omit = 'as.factor\\(season\\)',
          dep.var.labels = 'Meet the Spread')

stargazer(mp, covariate.labels = c('Party placebo', 'Lag changes in posession', 
                                   'Logged travel distance', 'East-west travel direction', 
                                   'Number hours rest time', 'Time of game during day', 
                                   'Home team effect', 'Logged travel distance * east-west', 
                                   'Constant'),
          omit = 'as.factor',
          dep.var.labels = 'Meet the Spread (NBA)')


## stargazer(m2, dep.var.labels = 'Demeanead Rebounds Per Minute',
##           covariate.labels = c('Continuous party measure',
##                                'Lag change in possessions',
##                                'Logged travel distance',
##                                'Number hours since last game',
##                                'Constant'))

stargazer(mpa1, tpa, tps, dep.var.labels = c('Points Admitted by Team', 'Team Points Admitted', 'Team Points Scored'),
          covariate.labels = c('Discrete party indicator',
                               'Continuous party measure',
                               'Lag change in possessions',
                               'Logged travel distance',
                               'Number hours since last game',
                               'Constant'))

stargazer(m3, m4, covariate.labels = c('Continuous measure of nightlife',
                                       'Nightlife (no weekend interaction)', 
                                       'Bookmaker\'s odds', 'Home-team effect',
                                       'Number of rest days', 'Logged travel distance',
                                       'Weekend', 'Constant'),
          omit = 'as.factor',
          dep.var.labels = 'Probability of Winning')


##################################################
### Dot plot of Party-Continuous by Team Location
##################################################
tmp <- nba.lines[, list(party.continuous = mean(log(nmusicians+1))),
                 by = team][order(party.continuous)]

pdf('writing/Party_by_Team_Location.pdf', width = 14, height = 8.5)
ticks = 1:nrow(tmp)
plot(x = ticks, 
     y = tmp$party.continuous, 
     main = 'Continuous measure of party by Team Location: NBA',
     ylab = 'Log sum of Musicians and Sound Recording Studios',
     xlab = '',
     axes = F)
axis(side = 1, at = ticks, labels = gsub('([a-z]{4,}) ', '\\1\n', tmp$team) %>% 
                               gsub('([a-z]{6})[a-z]{1,}', '\\1', .), las = 2, xpd = NA)
axis(side = 2)
dev.off()


tmp <- mlb.lines[, list(party.continuous = mean(log(nmusicians+1))),
                 by = team][order(party.continuous)][!is.na(party.continuous)]

pdf('writing/Party_by_Team_Location_mlb.pdf', width = 14, height = 8.5)
ticks = 1:nrow(tmp)
plot(x = ticks, 
     y = tmp$party.continuous, 
     main = 'Continuous measure of party by Team Location: MLB',
     ylab = 'Log sum of Musicians and Sound Recording Studios',
     xlab = '',
     axes = F)
axis(side = 1, at = ticks, labels = gsub('([a-z]{4,}) ', '\\1\n', tmp$team) %>% 
                               gsub('([a-z]{6})[a-z]{1,}', '\\1', .), las = 2, xpd = NA)
axis(side = 2)
dev.off()


##################################################
### Density of travel dist by party vs not
##################################################

pdf('writing/travel_dist_density_by_party.pdf', width = 12, height = 11)
plot(density(nba.lines[(party.discrete) & !is.na(travel.dist), travel.dist]), 
     xlim = c(-100, 18e2), lty = 1,
     main = 'Density of travel distance\nfor (non) party cities: NBA',
     xlab = 'Travel Distance (Miles)')
lines(density(nba.lines[!(party.discrete) & !is.na(travel.dist), travel.dist]), lty = 2)
legend(x = 15e2, y = 0.0015, legend = c('LA/NY', 'non-party cities'), lty = 1:2)
dev.off()

##################################################
### Table for (un)-correlated next day opponent, NBA.
##################################################

tmp <- nba.lines[!grepl("toronto", location) & !grepl("toronto", last.game.loc), .N, 
                 keyby = list(location, last.game.loc, 
                              cur.game = paste('Current game', 
                                               ifelse(location == team, 'at home', 'away')),
                              lst.game = paste('Last game',
                                               ifelse(last.game.loc == team, 'at home', 'away')))] %>% 
    na.omit

### We sort our table according to last.game.loc distance to NYC to show travel restrictions.
load(file = 'tmp_data/distance_matrix.RData')
dmat <- melt(dmat)
setDT(dmat)
setnames(dmat, c('last.game.loc', 'location', 'distance'))
dmat <- dmat[location == 'brooklyn nets', list(last.game.loc, distance)] 
setorder(dmat, distance)

tmp <- merge(tmp, dmat, by = 'last.game.loc')
setnames(tmp, 'distance', 'distance to NYC')
setorder(tmp, 'distance to NYC')
tmp[, last.game.loc := factor(last.game.loc, levels = dmat[order(distance, decreasing = T), last.game.loc])]
tmp[, location      := factor(location,      levels = dmat[order(distance, decreasing = T), last.game.loc])]

pdf('writing/next_day_opponent.pdf', width = 14, height = 14)
ggplot(tmp, aes(x = location, y = last.game.loc, fill = log(N))) +
    geom_tile() +
    facet_wrap(~lst.game + cur.game) + 
    labs(x = 'Current Game Location', y = 'Last Game Location', 
         title = 'Distribution of Next Day Opponent - NBA\n(sorted by distance to NYC)') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315, vjust = 0.5),
          plot.title = element_text(hjust=.5))
dev.off()

pdf('writing/next_day_opponent_on_tour.pdf', width = 14, height = 14)
ggplot(tmp[cur.game == 'Current game away' &
           lst.game == 'Last game away'], aes(x = location, y = last.game.loc, fill = log(N))) +
    geom_tile() +
    labs(x = 'Current Game Location', y = 'Last Game Location', 
         title = 'Distribution of Next Day Opponent - NBA\n(sorted by distance to NYC)') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315, vjust = 0.5),
          plot.title = element_text(hjust=.5))
dev.off()

##################################################
### Heat Map for MLB next day opponent
##################################################
tmp <- mlb.lines[!grepl("toronto", location) & !grepl("toronto", last.game.loc), .N, 
                 keyby = list(location, last.game.loc, 
                              cur.game = paste('Current game', 
                                               ifelse(location == team, 'at home', 'away')),
                              lst.game = paste('Last game',
                                               ifelse(last.game.loc == team, 'at home', 'away')))] %>% 
    na.omit

### We sort our table according to last.game.loc distance to NYC to show travel restrictions.
load(file = 'tmp_data/distance_matrix_mlb.RData')
dmat <- melt(dmat)
setDT(dmat)
setnames(dmat, c('last.game.loc', 'location', 'distance'))
dmat <- dmat[location == 'new york yankees', list(last.game.loc, distance)] 
setorder(dmat, distance)

tmp <- merge(tmp, dmat, by = 'last.game.loc')
setnames(tmp, 'distance', 'distance to NYC')
setorder(tmp, 'distance to NYC')
tmp[, last.game.loc := factor(last.game.loc, levels = dmat[order(distance, decreasing = T), last.game.loc])]
tmp[, location      := factor(location,      levels = dmat[order(distance, decreasing = T), last.game.loc])]

pdf('writing/next_day_opponent_mlb.pdf', width = 14, height = 14)
ggplot(tmp, aes(x = location, y = last.game.loc, fill = log(N))) + 
    geom_tile() +
    facet_wrap(~lst.game + cur.game) + 
    labs(x = 'Current Game Location', y = 'Last Game Location', 
         title = 'Distribution of Next Day Opponent - MLB') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315, vjust = 0.5),
          plot.title = element_text(hjust=.5))
dev.off()

##################################################
### Why drinking estabs doesn't work. (NBA)
##################################################

tmp <- nba.lines[, list(log.nmusicians = mean(log(nmusicians+1)),
                        log.population = mean(log(population+1), na.rm = T),
                        log.ndrinks    = mean(log(ndrinks+1))), by = team][order(log.nmusicians)]

pdf('writing/why_drinks_or_population_dont_work.pdf', width = 16, height = 8.5)
ticks = 1:nrow(tmp)
plot(x = ticks, 
     y = tmp$log.nmusicians,
     main = 'Why drinking establishments or population don\'t work as a proxy for nightlife',
     ylab = 'Log sum of Variable',
     xlab = '', axes = F, pch = 1, ylim = c(0, max(tmp$log.population,na.rm = T)), type = 'l')
points(x = ticks, y = tmp$log.ndrinks, pch = 16, col = 'purple', type = 'l')
points(x = ticks, y = tmp$log.population, pch = 18, col = 'orange', type = 'l')
### Houston has large population, but isn't necessarily known for nightlife.
segments(x0 = which(tmp$team == 'houston rockets'), 
         x1 = which(tmp$team == 'houston rockets'),
         y0 = -1,
         y1 = tmp[team == 'houston rockets', log.population], col = 'gray70', lty = 'dashed')
segments(x0 = which(tmp$team == 'houston rockets'), 
         x1 = length(ticks),
         y0 = tmp[team == 'houston rockets', log.population],
         y1 = tmp[team == 'houston rockets', log.population], col = 'gray70', lty = 'dashed')
abline(h = tmp[team == 'houston rockets', log.ndrinks], col = 'gray70', lty = 'dashed')
### New orleans is infamous for party, but Population places it at the bottom of the index.
segments(x0 = which(tmp$team == 'new orleans hornets'),
         x1 = which(tmp$team == 'new orleans hornets'), 
         y0 = -1, 
         y1 = tmp[team == 'new orleans hornets', log.population],col = 'gray70', lty = 'dashed')
segments(x0 = 0, x1 = which(tmp$team == 'new orleans hornets'),
         y0 = tmp[team == 'new orleans hornets', log.population],
         y1 = tmp[team == 'new orleans hornets', log.population], col = 'gray70', lty = 'dashed')
axis(side = 1, at = ticks, labels = gsub('([a-z]{4,}) ', '\\1\n', tmp$team) %>%
                               gsub('([a-z]{6})[a-z]{1,}', '\\1', .), las = 2, xpd = NA)
axis(side = 2)
legend(x = 1.5, y = 12.5, legend = c('Musicians and sound recording studios',
                                 'Drinking establishments and liquor stores',
                                 'Population'), pch = c(1, 16, 18), col = c('black', 'purple', 'orange'))
text(x = 3.6, y = 9.5, labels = paste("Correlation (alcohol):       ", 
                                      cor(tmp$log.nmusicians, tmp$log.ndrinks) %>%
                                      round(., 3)))
text(x = 3.6, y = 9, labels = paste("Correlation (population): ", 
                                     cor(tmp$log.nmusicians, tmp$log.population, use = 'complete')%>%
                                     round(., 3)))
points(x = c(which(tmp$team == 'new orleans hornets'),
             which(tmp$team == 'houston rockets'),
             which(tmp$team == 'houston rockets')),
       y = c(tmp[team == 'new orleans hornets', log.population],
             tmp[team == 'houston rockets', log.population],
             tmp[team == 'houston rockets', log.ndrinks]),
       type = 'p', col = 'red')
dev.off()

##################################################
### Proportion of meet the spread for nba/mlb.
##################################################

pdf('writing/meet_the_spread.pdf')
par(mfcol = c(2, 1))
tmp <- nba.lines[, list(meet.spread = mean(outcome == 'W')), 
          by = list(season, team)] %>% na.omit
hist(tmp$meet.spread, prob = T, main = paste('How often do teams meet the spread? (NBA)', 
                                             'By season-team, 2010-11 through 2016-17',
                                             sep = '\n'),
     xlab = 'Meet the Spread', breaks = 25)
lines(density(tmp$meet.spread, adjust = 2), col = 'orange', lty = 'dashed', lwd = 3)
### There should be 'no information' left over after conditioning on odds.
mlb.lines <- na.omit(mlb.lines)
noinf <- lm(I(team.score > opponent.score) ~ odds, data = mlb.lines)
preds <- fitted(noinf)
mlb.lines[, fitted := fitted(noinf)]
mlb.lines[, resid  := resid(noinf)]
tmp <- mlb.lines[, .(avg.residual = mean(resid)), by = .(fitted = round(fitted, 3))]
plot(x = tmp$fitted, y = tmp$avg.residual, 
     xlab = 'Fitted Probability', ylab = 'Average Residual', 
     main = paste('After conditioning on bookkeeper\'s odds (MLB),',
                  'successful prediction is a coin-flip', sep = '\n'))
abline(lm(avg.residual ~ fitted, tmp), col = 'orange', lty = 'dashed', lwd = 3)
dev.off()
