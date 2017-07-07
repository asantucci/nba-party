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
### Inputs: 
###
### Dependencies: 
###
################################################################################
################################################################################


Sys.setlocale('LC_ALL','C')  # Needed for population data (within area.title variable)
require(hangover)

require(data.table)
require(ggplot2)
require(magrittr)
require(sampling)
require(stargazer)
set.seed(4052017)

### Load data.
load(file = 'tmp_data/covers_lines.RData')
load(file = 'tmp_data/population_by_MSA_year.RData')
espn <- fread('tmp_data/espn_player_data.csv')
espn[, date := as.Date(date)]

##############################
### Assemble NBA Data.
##############################

load(file = 'tmp_data/studios_and_artists.RData')
musicians <- copy(dt)
rm(dt)
setnames(musicians, 'variable', 'nmusicians')

load(file = 'tmp_data/alcohol_and_drinking.RData')
drinking <- copy(dt)
rm(dt)
setnames(drinking, 'variable', 'ndrinks')

### Bring together musicians and drinking establishments.
bls <- merge(musicians, drinking, on = intersect(names(musicians), names(drinking)))
bls[, area.title := gsub(' msa$', '', area.title, ignore.case = T)]

### Geocode nba team locations. Prepare data for merge.
## locs <- MyGeoCode(unique(lines$team), 'nba')
## save(locs, file = 'tmp_data/nba_team_locations.RData')
load('tmp_data/nba_team_locations.RData')  # <-- locs.
states <- data.table(state = state.name, state.abb = state.abb)
states <- rbind(states, data.frame(state = 'District of Columbia', state.abb = 'DC'))
locs <- locs[states, on = 'state', nomatch = 0]  # Gets rid of Toronto. That's ok!

### Attach team-location information to lines data.
lines <- lines[locs, on = 'team']  # Here, we lose Toronto (that's OK).


### We can't simply merge. We have to look for the county within the MSA.
### For each location, we fetch a panel of corresponding musicians data, 2010-2016.
LookupLocation <- function(loc, st, data, team = NULL) {
    sb <- data[grepl(loc, area.title, ignore.case = T) & grepl(st,  area.title)]
    sb[, `:=`(locality = loc, state = st)]
    if (!is.null(team)) sb[, team := team]
    return(sb)
}

### Attach nba team-names to BLS data.
demog <- mapply(LookupLocation, loc = locs$locality, st = locs$state.abb, team = locs$team,
                MoreArgs = list(data = bls), SIMPLIFY = F) %>% rbindlist
demog <- merge(demog, pop, by = c('area.title', 'year'), all.x = T)
setnames(demog, 'state', 'state.abb')
setcolorder(demog, c('team', 'year', 'locality', 'state.abb', 'nmusicians', 'ndrinks',
                     'population', 'area.title'))


### Merge bls data with nba lines.
setnames(demog, 'year', 'season')
demog[, season := season + 1]
nba.lines <- lines[demog, on = c('team', 'season', 'locality', 'state.abb', 'season')]

### Exclude playoff games.
setkey(nba.lines, season, team, date)
nba.lines <- nba.lines[, head(.SD, n=82), by=list(season,team)]


##############################
### Party Variable
##############################

HOURS <- 24
## nba.lines[, party := ifelse(nhours.lgame <= HOURS & last.game.loc != team & weekdays(date) %in% c('Saturday', 'Sunday'), log(nmusicians+1), 0)]

### We create a discrete indicator for party based on LA or NY. As well as a placebo (longer rest)
nba.lines[, party.discrete := grepl("(los angeles)|(new york)|(brooklyn)", last.game.loc) &
                nhours.lgame <= HOURS &
                last.game.loc != team]
nba.lines[, party.placebo := grepl("(los angeles)|(new york)|(brooklyn)", last.game.loc) &
                nhours.lgame > HOURS &
                last.game.loc != team]

### We then create a continuous measure of nightlife. We rescale to 0-1 so it's comparable.
nba.lines[, party := ifelse(nhours.lgame <= HOURS & last.game.loc != team, log(nmusicians+1), 0)]
nba.lines[, party := party / max(party, na.rm = T)]

##############################
### Lag changes in possesion
##############################

nba.lines[, simple.date := as.character(date) %>% substr(., 1, 10) %>% as.Date]
data <- merge(nba.lines, espn,
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
nba.lines <- merge(nba.lines, changes, by = c('season', 'location', 'date'))

setkey(nba.lines, season, team, date)
nba.lines[, lag.chg.pos := c(NA, lag(ttl.chg.pos)[1:.N-1]), by = list(season, team)]

##############################
### Init model
##############################

nba.lines[, ew := sin(travel.dir %% 360 / 360 * 2 * pi)]
## plot(nba.lines$travel.dir, nba.lines$ew)  # for visual inspection.

m0 <- glm(outcome == 'W' ~ party.discrete + lag.chg.pos + log(travel.dist+1)*ew + 
             nhours.lgame + I(hour(date)) + I(team==location),
         data = nba.lines[season < 2017 & !is.na(party)], family = 'binomial',
         na.action = 'na.exclude')

m1 <- update(m0, . ~ party + . - party.discrete)

summary(m1)

## nba.lines[,.N/2,by=list(season)]
stargazer(m0, m1, covariate.labels = c('Party discrete', 'Party continuous', 
                                       'Lag changes in posession', 
                                       'Logged travel distance', 
                                       'East-west travel direction', 
                                       'Number hours rest time', 
                                       'Time of game during day', 
                                       'Home team effect', 
                                       'Logged travel distance * east-west', 
                                       'Constant'),
          dep.var.labels = 'Meet the Spread')

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
## text(x = ticks, y = 1.25, labels = tmp$team, srt = 90)
axis(side = 2)
dev.off()

##################################################
### Histogram of travel dist by party vs not
##################################################
pdf('writing/travel_dist_by_party.pdf', width = 11, height = 8.5)
par(mfrow = c(2,1))
hist(x = nba.lines[party.discrete == F, travel.dist], 
     main = 'Histogram of Travel Distance by Last Game Location', xlab = '')
legend(x = 2250, y = 3500, legend = c('Non Party Cities', 'LA and NY'), 
       col = c('white', 'black'), fill = c(F, T))
hist(x = nba.lines[party.discrete == T, travel.dist], 
     main = '', col = 'black', xlab = 'Travel Distance')
dev.off()


pdf('writing/travel_dist_density_by_party.pdf', width = 12, height = 11)
plot(density(nba.lines[(party.discrete) & !is.na(travel.dist), travel.dist]), 
     xlim = c(-100, 18e2), lty = 1,
     main = 'Density of travel distance\nfor (non) party cities: NBA',
     xlab = 'Travel Distance (Miles)')
lines(density(nba.lines[!(party.discrete) & !is.na(travel.dist), travel.dist]), lty = 2)
legend(x = 15e2, y = 0.0015, legend = c('LA/NY', 'non-party cities'), lty = 1:2)
dev.off()

##################################################
### Placebo model
##################################################
mp <- update(m0, . ~ party.placebo + . - party.discrete)

# mp
stargazer(mp, covariate.labels = c('Party placebo', 'Lag changes in posession', 
                                   'Logged travel distance', 'East-west travel direction', 
                                   'Number hours rest time', 'Time of game during day', 
                                   'Home team effect', 'Logged travel distance * east-west', 
                                   'Constant'),
          dep.var.labels = 'Meet the Spread (NBA)')

##################################################
### Table for (un)-correlated next day opponent
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

##############################
### Player specific model
##############################

nba.lines[, game.time := hour(date)]

m2 <- lm(demeaned.reb.per.min ~ party + lag.chg.pos + log(travel.dist+1) + nhours.lgame,
   data = data[last.game.loc != team] %>% na.omit)
summary(m2)

tpa <- lm(team.pts.admitted ~ party + lag.chg.pos + log(travel.dist+1) + nhours.lgame, 
   data=nba.lines[last.game.loc!=team] %>% na.omit)
tpo <- update(tpa, team.pts.scored ~ .)

### Points  allowed as afunction of last game location (by party)
nba.lines[, demeaned.pts.admitted := team.pts.admitted - mean(team.pts.admitted), by = list(season, team)]
nba.lines[, party.discrete := grepl("(los angeles)|(new york)|(brooklyn)", last.game.loc) &
                nhours.lgame <= HOURS &
                last.game.loc != team]

nba.lines[, mean(demeaned.pts.admitted), by = list(last.game.loc, party.discrete)][order(V1)]
mpa1 <- lm(demeaned.pts.admitted ~ party.discrete, data = nba.lines)


stargazer(m2, dep.var.labels = 'Demeanead Rebounds Per Minute',
          covariate.labels = c('Continuous party measure',
                               'Lag change in possessions',
                               'Logged travel distance',
                               'Number hours since last game',
                               'Constant'))

stargazer(mpa1, tpa, tpo, dep.var.labels = c('Points Admitted by Team', 'Team Points Admitted', 'Team Points Scored'),
          covariate.labels = c('Discrete party indicator',
                               'Continuous party measure',
                               'Lag change in possessions',
                               'Logged travel distance',
                               'Number hours since last game',
                               'Constant'))

##############################
### MLB
##############################

load(file = 'tmp_data/covers_lines_mlb.RData')
locs <- MyGeoCode(unique(lines$team), 'mlb')

states <- data.table(state = state.name, state.abb = state.abb)
states <- rbind(states, data.frame(state = 'District of Columbia', state.abb = 'DC'))
locs  <- locs[states, on = 'state', nomatch = 0]  # Here, we lose Toronto (that's OK).
lines <- lines[locs, on = 'team'] 


### Attach mlb team-names to BLS data.
demog <- mapply(LookupLocation, loc = locs$locality, st = locs$state.abb, team = locs$team,
                MoreArgs = list(data = bls), SIMPLIFY = F) %>% rbindlist
demog <- merge(demog, pop, by = c('area.title', 'year'), all.x = T)
setnames(demog, 'state', 'state.abb')
setcolorder(demog, c('team', 'year', 'locality', 'state.abb', 'nmusicians', 'ndrinks',
                     'population', 'area.title'))


### Merge bls data with nba lines.
setnames(demog, 'year', 'season')
demog[, season := season]
mlb.lines <- lines[demog, on = c('team', 'season', 'locality', 'state.abb', 'season')]

### Exclude playoff games.
setkey(mlb.lines, season, team, date)
mlb.lines <- mlb.lines[, head(.SD, n=162),by=list(season,team,date)] #Exclude playoff games

mlb.lines[, party := ifelse(team != location & weekend == 1, log(nmusicians+1), 0)]
mlb.lines[, party := party / max(party, na.rm = T)]

### See what happens if we don't interact with weekend, i.e. no weekend effect.
mlb.lines[, party.now := ifelse(team != location, log(nmusicians+1), 0)]
mlb.lines[, party.now := party.now / max(party.now, na.rm = T)]

m3 <- glm(I(team.score > opponent.score) ~ party + odds + I(team == location) + 
             ndays.lgame + log(travel.dist+1) + weekend,
         data = mlb.lines[season < 2017], family = 'binomial')
summary(m3)
m4 <- update(m3, . ~ party.now + . - party)

mlb.lines[, party.discrete := grepl("(los angeles)|(new york)", last.game.loc) & ndays.lgame == 1 & weekend == T]

stargazer(m3, m4, covariate.labels = c('Continuous measure of nightlife',
                                       'Nightlife (no weekend interaction)', 
                                       'Bookmaker\'s odds', 'Home-team effect',
                                       'Number of rest days', 'Logged travel distance',
                                       'Weekend', 'Constant'), 
          dep.var.labels = 'Probability of Winning')

##################################################
### Heat Map for next day opponent
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
### Party by team location
##################################################
## load(file = 'tmp_data/studios_and_artists_mlb.RData')
## setnames(dt, 'variable', 'nmusicians')

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

### Meet the spread for both NBA and MLB
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


##############################
### Defunct.
##############################
if (0) {
    m <- glm(I(team.score > opponent.score) ~ party + odds,
             data = mlb.lines[season < 2016], family = 'binomial')
    p <- predict(m, newdata = mlb.lines[season == 2016], type = 'response')
    idx <- which(p < .45)
    p1 <- mapply(Predict, prediction = p[idx], 
                 vegas = mlb.lines[season == 2016, odds][idx],
                 MoreArgs=list(threshold = 0.01))
    b <- mapply(Bet, our.prediction = p1,
                actual.outcome = mlb.lines[season == 2016, team.score > opponent.score][idx],
                odds = mlb.lines[season == 2016, odds][idx], SIMPLIFY = T)
    
### the following plot shows sufficient deviation from house odds to motivate
### an optimized betting strategy.
    hist(p- mlb.lines[season == 2016, odds])
    
    tholds <- seq(.01, .1, by = .005)
    tmp <- sapply(tholds, function(t) {
        p1 <- mapply(Predict, prediction = p[idx], 
                     vegas = mlb.lines[season == 2016, odds][idx],
                     MoreArgs=list(threshold = t))
        b <- mapply(Bet, our.prediction = p1,
                    actual.outcome = mlb.lines[season == 2016, team.score > opponent.score][idx],
                    odds = mlb.lines[season == 2016, odds][idx], SIMPLIFY = T)
        return(c(sum(b, na.rm = T) / sum(!is.na(b)), sum(!is.na(b)))) # Avg profit per game.
    })

    plot(x = tholds, y = tmp[1, ], main = 'Return per game\nas a function of threshold', xlab = 'threshold', ylab = 'average profit')
    text(x = tholds, y = tmp[1, ], labels = tmp[2, ], pos = 1)
}

### plot return per game as a function of difference in odds between us and vegas.

### Musicians by area.
## load(file = 'tmp_data/studios_and_artists.RData')
## tmp <- demog[, list(log.nmusicians = log(nmusicians + 1),
##                       log.ndrinks    = log(ndrinks    + 1),
##                       log.population = log(population + 1)), by = locality][order(log.nmusicians)]
## points(demog[, log(nmusicians + 1)])
