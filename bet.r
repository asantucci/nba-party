
require(data.table)
load(file = 'tmp_data/mlb.RData')  # mlb.lines

### Subset to today's games.
schedule <- mlb.lines[as.Date(date) == Sys.Date(), .(date, team, opponent, matchup, line, party)]
write.csv(schedule, 'tmp_data/betting/2017_07_18.csv', row.names = F)


### Manually input lines.
### Then, and only then, load this same file back into R :-).
schedule <- fread('tmp_data/betting/2017_07_18.csv')
# c(-147, -110, -124, -110, -110, -310, 127, -147, -145, -109, -190, -111, -131, -310, -135, -125, 125, -103, -145, -106, 115, -145, 125, 127, 165, -117, -114, 104, 111)

### Determine odds of winning.
schedule[, odds := ifelse(line < 0, 100 / (-line + 100), line / (line + 100))]

### Just for today.
m <- glm(I(team.score > opponent.score) ~ party + odds,
         data = mlb.lines, family = 'binomial')
schedule[, prediction := predict(m, newdata = schedule, type = 'response')]
schedule <- schedule[, .SD[which.max(abs(prediction - .5))], by = list(matchup, date)]

schedule[, bettingStrategy := mapply(hangover::Predict,
                                     prediction = schedule[, prediction],
                                     vegas = schedule[, odds], 
                                     MoreArgs = list(threshold = 0.02))] # 0.02 was chosen ad-hoc.

write.csv(schedule, file = 'tmp_data/betting/2017_07_18_bets.csv', row.names = F)

##############################
### Year to date performance
##############################
m <- glm(I(team.score > opponent.score) ~ party + odds,
         data = mlb.lines[year(date) < 2017], family = 'binomial')

mlb.lines[year(date) == 2017,
          prediction := predict(m, newdata = mlb.lines[year(date) == 2017], type = 'response')]

### Take the stronger bet
mlb.lines <- mlb.lines[, .SD[which.max(abs(prediction - .5))], by = list(matchup, date)]

mlb.lines[year(date) == 2017,
          prediction := mapply(hangover::Predict,
                               prediction = mlb.lines[year(date) == 2017, prediction],
                               vegas = mlb.lines[year(date) == 2017, odds], 
                               MoreArgs = list(threshold = 0.02))] # 0.02 was chosen ad-hoc.

mlb.lines <- mlb.lines[!is.na(prediction)]
### Translate prediction into a monetary gain or loss based on odds and actual outcome.
mlb.lines[, bet := mapply(hangover::Bet, our.prediction = prediction, 
                          actual.outcome = team.score > opponent.score,
                          odds = odds)]
### Plot the cumulative sum of profits over the course of the season.
mlb.lines[, running.prof := cumsum(bet)]    

cat(paste0(sum(mlb.lines$bet), '\n'))
plot(x = 1:nrow(mlb.lines), y = mlb.lines$running.prof, 
     main = paste0('Profit over the ', 2017, ' season: $', 
                   format(round(sum(mlb.lines$bet), digits = -1), big.mark = ','), '\n',
                   'Training on 2011-', 2017-1, ' data'),
     xlab = 'Game Number', ylab = 'Profit', type = 'l')
abline(a = 0, b = 0, lty = 'dashed', col = 'orange', lwd = 3)
points(x = which.min(mlb.lines$running.prof), y = min(mlb.lines$running.prof), type = 'p')
text(x = which.min(mlb.lines$running.prof), y = min(mlb.lines$running.prof), 
     labels = round(min(mlb.lines$running.prof)), pos = 4)
