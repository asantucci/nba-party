################################################################################
################################################################################
################################################################################
###
### Title: Clean MLB Covers
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

### Load in data files.
files <- list.files('tmp_data/covers_mlb/', full.names = T)
data <- lapply(files, fread) %>% rbindlist(., fill = T)
data[, c('team', 'opponent') := tstrsplit(matchup, split = '-')]

data <- data[, list(date, matchup, team, opponent, Line, score)]

### Expand team abbreviations.
abbrs <- data[, unique(c(unique(team), unique(opponent))) %>% sort]
fulls <- c('arizona diamondbacks', 'atlanta braves', 'baltimore orioles', 'boston redsox',
           'chicago cubs', 'chicago whitesox', 'cincinati reds', 'cleveland indians',
           'colorado rockets', 'detroit tigers', 'houston astros', 'kansas city royals',
           'los angeles angels', 'los angeles dodgers', 'miami marlins', 'milwaukee brewers',
           'minnesota twins', 'new york mets', 'new york yankees', 'oakland athletics',
           'philadelphia phillies', 'pittsburgh pirates', 'san diego padres', 'seattle mariners',
           'san francisco giants', 'st louis cardinals', 'tampa bay rays', 'texas rangers',
           'toronto blue jays', 'washington nationals')
abbrs <- data.table(abbr = abbrs, team.full = fulls)
data <- merge(data, abbrs, by.x = 'team', by.y = 'abbr', all = T)
setnames(abbrs, 'team.full', 'opponent.full')
data <- merge(data, abbrs, by.x = 'opponent', by.y = 'abbr', all = T)
data[, c('matchup', 'team', 'opponent') := NULL]
setnames(data, gsub('\\.full', '', tolower(names(data))))
setcolorder(data, c('date', 'team', 'opponent', 'line', 'score'))

data[, line := gsub(' \\(Open\\)$', '', line) %>% as.numeric]
data[, location := opponent] ### See http://www.covers.com/sports/MLB/matchups?selectedDate=2011-4-01.
data <- data[!is.na(line)]

data[, odds := ifelse(line < 0, 100 / (-line + 100), line / (line + 100))]

### The line is applied to 'team', and the sign indicates whether they are favored.
## data[, mean(team.score > opponent.score), by = sign(line)]
## lm(I(team.score > opponent.score) ~ odds, data) %>% summary

data[, weekday := weekdays(as.Date(date))]

### Creating panel data-set.
lines.dup <- copy(data)
lines.dup[, `:=`(team = opponent, opponent = team,
                 score = strsplit(score, split = '-') %>%
                     lapply(., rev) %>%
                     sapply(., paste, collapse = '-'),
                 odds = 1 - odds,
                 location = location)]

lines <- rbind(data, lines.dup)

load(file = 'tmp_data/game_info_mlb.RData')
game.info <- game.info[!grep("Vs\\..*Vs\\.", matchup)]
game.info[, c('team.nick', 'opponent.nick') := tstrsplit(matchup, split = ' Vs\\. ')]

### Get nick-names for lines data, so we can merge with game info.
setnames(abbrs, c('abbr', 'team'))
abbrs[, nick := gsub('.* ([^ ]+)$', '\\1', team)]
abbrs[nick == 'rays', nick := 'bay rays']
abbrs[nick == 'jays', nick := 'blue jays']
setnames(abbrs, paste('team', names(abbrs), sep = '.'))
lines <- merge(lines, abbrs, by.x = 'team', by.y = 'team.team', all.x = T)
setnames(abbrs, gsub('team', 'opponent', names(abbrs)))
lines <- merge(lines, abbrs, by.x = 'opponent', by.y = 'opponent.opponent', all = T)

write.csv(abbrs, file = 'mlb_abbrs.csv', row.names = F)

game.info[, team.nick := gsub('(.*) sox', '\\1sox', team.nick, ignore.case = T) %>% tolower]
game.info[, opponent.nick := gsub('(.*) sox', '\\1sox', opponent.nick, ignore.case = T) %>% tolower]
setnames(game.info, 'location', 'stadium')
game.info[, date := as.Date(date, format = '%A, %B %d, %Y')]
lines[, date := as.Date(date)]
lines <- merge(lines, game.info, by = c('team.nick', 'opponent.nick', 'date'), all = T)

lines[, season := year(date)]
setkey(lines, team, season, date)

lines[, ndays.lgame   := c(NA, diff(date)),              by = list(team, season)]
lines[, last.game.loc  := c(NA, lag(location)[1:.N-1]),   by = list(team, season)]

lines[, c('team.score', 'opponent.score') := tstrsplit(score, split = '-')]

party.cities <- '(los angeles)|(new york)'
lines[, weekend := weekday %in% c('Saturday', 'Sunday')]

load(file = 'tmp_data/nmusician_estabs_mlb.RData')
lines <- merge(lines, musicians,
               by.x = c('season', 'last.game.loc'),
               by.y = c('season', 'team'), all.x = T)
lines[, party := ifelse(last.game.loc != team, nmusicians, 0)]
#lines[, party := ifelse(grepl(party.cities, last.game.loc) & team != last.game.loc, 1, 0)]
m <- glm(I(team.score > opponent.score) ~ party + I(team == location) + odds,
         data = lines, family = 'binomial')

lines <- lines[season < 2017]
require(sampling)
set.seed(04212017)
s <- strata(lines, c('season', 'last.game.loc'),
       size = rep(50, 30*length(unique(lines$season))), method = 'srswor')
bstrap.sample <- lines[s$ID_unit]
train.sample <- lines[-s$ID_unit]
cf <- glm(I(team.score > opponent.score) ~ 0 + last.game.loc, data = bstrap.sample) %>%
    coef
cf <- data.table(last.game.loc = names(cf) %>% gsub('last.game.loc', '', .), cf)
train.sample <- train.sample[cf, on = 'last.game.loc']

glm(I(team.score > opponent.score) ~ cf + odds + I(team == location), data = train.sample) %>% summary

### Distance traveled. nhours last game.

lines[, mean(team.score > opponent.score), by = last.game.loc][order(V1)]










