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
### Dependencies: data.table, ggplot2, magrittr, stargazer
###
################################################################################
################################################################################

##################################################
### Set up workspace.
##################################################

require(data.table)
require(ggplot2)
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
## m2 <- lm(reb.per.min ~ party + lag.chg.pos + log(travel.dist+1) + nhours.lgame +
##              as.factor(season) + player*as.factor(season),
##          data = espn[last.game.loc != team] %>% na.omit)

## require(lfe)
## m2 <- felm(reb.per.min ~ party.discrete + lag.chg.pos + log(travel.dist+1) + nhours.lgame +
##              as.factor(season) + player,
##          data = espn[last.game.loc != team] %>% na.omit)

### Total points admitted and scored.
tpa <- lm(team.pts.admitted ~ party + lag.chg.pos + log(travel.dist+1) + nhours.lgame +
              I(team == location) + team,
          data = nba.lines[last.game.loc != team] %>% na.omit)
mpa1 <- update(tpa, . ~ party.discrete + . - party)
tps <- update(tpa, team.pts.scored ~ . + team)


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
pdf('cardinal_analytx_presentation/figures/mlb_bets_by_season.pdf', width = 18, height = 13)
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
         xlab = 'Game Number', ylab = '', type = 'l',
         cex.main = 2.25, cex.axis = 2, cex.lab = 2)
    mtext(text = 'Profit', side = 2, line = 2.5, cex = 2)
    abline(a = 0, b = 0, lty = 'dashed', col = 'orange', lwd = 3)
    points(x = which.min(tmp$running.prof), y = min(tmp$running.prof), type = 'p')
    text(x = which.min(tmp$running.prof), y = min(tmp$running.prof), 
         labels = round(min(tmp$running.prof)), pos = 4, cex = 2)
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
          keep = 'party',
          dep.var.labels = 'Meet the Spread')

stargazer(mp, covariate.labels = c('Party placebo', 'Lag changes in posession', 
                                   'Logged travel distance', 'East-west travel direction', 
                                   'Number hours rest time', 'Time of game during day', 
                                   'Home team effect', 'Logged travel distance * east-west', 
                                   'Constant'),
          keep = 'party',
          dep.var.labels = 'Meet the Spread (NBA)')


## stargazer(m2, dep.var.labels = 'Demeanead Rebounds Per Minute',
##           covariate.labels = c('Continuous party measure',
##                                'Lag change in possessions',
##                                'Logged travel distance',
##                                'Number hours since last game',
##                                'Constant'))

stargazer(mpa1, tpa, tps,
          dep.var.labels = c('Team Points Admitted', 'Team Points Scored'),
          covariate.labels = c('Discrete party indicator',
                               'Continuous party measure',
                               'Lag change in possessions',
                               'Logged travel distance',
                               'Number hours since last game',
                               'Home team effect',
                               'Constant'),
          keep = 'party')

stargazer(m3, m4, covariate.labels = c('Continuous measure of nightlife',
                                       'Nightlife (no weekend interaction)', 
                                       'Bookmaker\'s odds', 'Home-team effect',
                                       'Number of rest days', 'Logged travel distance',
                                       'Weekend', 'Constant'),
          keep = 1:2,
          dep.var.labels = 'Probability of Winning')


##################################################
### Dot plot of Party-Continuous by Team Location
##################################################

data <- rbind(nba.lines[, list(avg.party = mean(log(nmusicians+1), na.rm = T),
                               sport = 'nba'), by = .(team, locality, state.abb)],
              mlb.lines[, list(avg.party = mean(log(nmusicians+1), na.rm = T),
                               sport = 'mlb'), by = .(team, locality, state.abb)])
setkey(data, avg.party)
data[, idx := .GRP, by = locality]

pdf('cardinal_analytx_presentation/figures/Party_by_Team_Location.pdf', width = 18, height = 12)
plot(x = data[sport == 'nba', jitter(idx)],
     y = data[sport == 'nba', avg.party],
     main = 'Continuous measure of party by location',
     ylab = '',
     xlab = '',
     axes = F, pch = 24, col = 'orange', lwd = 5, cex.main = 3, cex.lab = 2)
axis(side = 1, at = data[sport == 'nba', idx], line = -1,
     ### Shorten team-names. Take names 4+ letters long, add a new-line.
     labels = gsub('([a-z]{3,}) ', '\\1\n', data[sport == 'nba', locality], ignore.case = T) %>%
         gsub('([a-z]{6})[a-z]{1,}', '\\1', ., ignore.case = T), las = 2, xpd = NA,
     cex.lab = 1.5, cex.axis = 1.5)
axis(side = 2, cex.axis = 1.5, line = -1)
mtext(text = 'Log sum of Musicians and Sound Recording Studios', side = 2, line = 2.25, cex = 2)
points(x = data[sport == 'mlb', jitter(idx)],
       y = data[sport == 'mlb', avg.party], pch = 25, col = 'purple', lwd = 5)
axis(side = 1, at = data[sport == 'mlb', idx], line = -1,
     labels = gsub('([a-z]{3,}) ', '\\1\n', data[sport == 'mlb', locality], ignore.case = T) %>% 
         gsub('([a-z]{6})[a-z]{1,}', '\\1', ., ignore.case = T), las = 2, xpd = NA,
     cex.lab = 1.5, cex.axis = 1.5)
legend(x = data[, min(idx)], y = data[, max(avg.party)],
       legend = c('NBA', 'MLB'),
       fill = c('orange', 'purple'), cex = 2, title = 'Legend')
dev.off()

##################################################
### Density of travel dist by party vs not
##################################################


pdf('cardinal_analytx_presentation/figures/travel_dist_density_by_party.pdf', width = 15, height = 13)
plot(density(nba.lines[(party.discrete) & !is.na(travel.dist), travel.dist]), 
     xlim = c(-100, 18e2), lty = 1,
     main = 'Density of travel distance for (non) party cities: NBA',
     xlab = 'Travel Distance (Miles)',
     cex.main = 3, cex.axis = 2, cex.lab = 2, ylab = '')
lines(density(nba.lines[!(party.discrete) & !is.na(travel.dist), travel.dist]), lty = 2)
legend(x = 12e2, y = 0.0015, legend = c('LA/NY', 'non-party cities'), lty = 1:2, cex = 2,
       title = 'Last game location')
mtext(text = 'Density', side = 2, line = 2.4, las = 3, cex = 2)
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

breaks <- 2^(0:6)

pdf('cardinal_analytx_presentation/figures/next_day_opponent.pdf', width = 14, height = 14)
ggplot(tmp, aes(x = location, y = last.game.loc, fill = N)) +
    geom_tile() +
    scale_fill_gradient(name = 'Count', trans = 'log', breaks = breaks) +
    facet_wrap(~lst.game + cur.game) + 
    labs(x = 'Current Game Location', y = 'Last Game Location', 
         title = 'Distribution of Next Day Opponent - NBA\n(axes sorted by distance to NYC)') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315, vjust = 0.5),
          plot.title = element_text(hjust=.5))
dev.off()

pdf('cardinal_analytx_presentation/figures/next_day_opponent_on_tour.pdf', width = 16, height = 12)
ggplot(tmp[cur.game == 'Current game away' &
           lst.game == 'Last game away'], aes(x = location, y = last.game.loc, fill = N)) +
    geom_tile() +
    scale_fill_gradient(name = 'Count', trans = 'log', breaks = breaks) +
    labs(x = 'Current Game Location', y = 'Last Game Location', 
         title = 'Distribution of Next Day Opponent - NBA\n(sorted by distance to NYC)') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315, vjust = 0.5, size = rel(2)),
          axis.text.y = element_text(size = rel(2)),
          plot.title  = element_text(size = rel(3), hjust=.5),
          legend.key.size = unit(3, 'line'),
          legend.text = element_text(size = rel(2)),
          legend.title = element_text(size = rel(2)),
          axis.title.x = element_text(size = rel(3)),
          axis.title.y = element_text(size = rel(3)))
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

pdf('cardinal_analytx_presentation/figures/next_day_opponent_mlb.pdf', width = 14, height = 14)
ggplot(tmp, aes(x = location, y = last.game.loc, fill = N)) + 
    geom_tile() +
    scale_fill_gradient(name = 'Count', trans = 'log', breaks = breaks) +
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

pdf('cardinal_analytx_presentation/figures/why_drinks_or_population_dont_work.pdf', width = 16, height = 12)
ticks = 1:nrow(tmp)
plot(x = ticks,
     y = tmp$log.nmusicians,
     main = 'Why drinking establishments or population don\'t work as a proxy',
     ylab = '',
     xlab = '', axes = F, pch = 1, ylim = c(0, max(tmp$log.population,na.rm = T)), type = 'l',
     cex.main = 3, cex.axis = 2, cex.lab = 2)
mtext(text = 'Log sum of Variable', side = 2, line = 2.5, cex = 2)
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
                               gsub('([a-z]{6})[a-z]{1,}', '\\1', .), las = 2, xpd = NA,
     cex.axis = 1.25)
axis(side = 2)
legend(x = 1, y = 12.5, legend = c('Musicians and sound recording studios',
                                 'Drinking establishments and liquor stores',
                                 'Population'), pch = c(1, 16, 18), col = c('black', 'purple', 'orange'), cex = 1.5)
text(x = 3.965, y = 9.5, labels = paste("Correlation (alcohol):      ", 
                                      cor(tmp$log.nmusicians, tmp$log.ndrinks) %>%
                                      round(., 3)), cex = 1.75)
text(x = 3.8, y = 9, labels = paste("Correlation (population): ", 
                                     cor(tmp$log.nmusicians, tmp$log.population, use = 'complete')%>%
                                     round(., 3)), cex = 1.75)
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

pdf('cardinal_analytx_presentation/figures/meet_the_spread.pdf')
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
## noinf <- lm(I(team.score > opponent.score) ~ odds, data = mlb.lines)
noinf <- glm(I(team.score > opponent.score) ~ odds, data = mlb.lines, family = 'binomial')
mlb.lines[, fitted := fitted(noinf)]
mlb.lines[, resid  := resid(noinf)]
tmp <- mlb.lines[, .(avg.residual = mean(resid)), by = .(fitted = round(fitted, 3))]
plot(tmp$fitted, tmp$avg.residual, 
     xlab = 'Fitted Probability', ylab = 'Average Residual', 
     main = paste('After conditioning on bookkeeper\'s odds (MLB),',
                  'successful prediction is a coin-flip', sep = '\n'))
abline(lm(resid ~ fitted, mlb.lines), col = 'orange', lty = 'dashed', lwd = 3)
dev.off()

##################################################
### Distribution of point spreads.
##################################################
h <- hist(nba.lines$line, breaks = 32, plot = F)
cuts <- cut(h$breaks, breaks = c(-Inf, -3, 3, Inf))
pdf('cardinal_analytx_presentation/figures/distribution_point_spreads_nba.pdf', width = 19, height = 13)
plot(h, col = c('white', 'orange', 'white')[cuts],
     main = paste('Distribution of point-spreads in the NBA',
                  '2010-11 through 2016-17', sep = '\n'),
     xlab = 'Point-spread', ylab = '',
     cex.main = 2.5, cex.axis = 2, cex.lab = 2)
mtext(text = 'Frequency', side = 2, line = 2.7, las = 3, cex = 2)
dev.off()

sd(nba.lines$line)
