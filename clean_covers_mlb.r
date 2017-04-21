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
data <- lapply(files, fread) %>% rbindlist
data[, c('team', 'opponent') := tstrsplit(matchup, split = '-')]

### Expand team abbreviations.
abbrs <- data[, unique(c(unique(team), unique(opponent))) %>% sort]
fulls <- c('arizona diambondbacks', 'atlanta braves', 'baltimore orioles',
           'boston redsox', 'chicago cubs', 'chicago whitesox', 'cincinati reds',
           'cleveland indians', 'colorado rockets', 'detroit tigers',
           'houston astros', 'kansas city royals', 'los angeles angels',
           'los angeles dodgers', 'miami marlins', 'milwaukee brewers',
           'minnesota twins', 'new york mets', 'new york yankees',
           'oakland athletics', 'philadelphia phillies', 'pittsburgh pirates',
           'san diego padres', 'seattle mariners', 'san francisco giants',
           'st louis cardinals', 'tampa bay rays', 'texas rangers',
           'toronto blue jays', 'washington nationals')
abbrs <- data.table(abbr = abbrs, team.full = fulls)
setkey(abbrs, abbr)
data <- merge(data, abbrs, by.x = 'team', by.y = 'abbr', all = T)
setnames(abbrs, 'team.full', 'opponent.full')
data <- merge(data, abbrs, by.x = 'opponent', by.y = 'abbr', all = T)
data[, c('opponent', 'team', 'Over.Under', 'BetOnline.ag', 'matchup') := NULL]
setnames(data, gsub('\\.full', '', tolower(names(data))))
setcolorder(data, c('date', 'team', 'opponent', 'line', 'score'))

data[, line := gsub(' \\(Open\\)$', '', line) %>% as.numeric]
data[, location := opponent] ### See http://www.covers.com/sports/MLB/matchups?selectedDate=2011-4-01.

### Creating panel data-set.
lines.dup <- copy(data)
lines.dup[, `:=`(team = opponent, opponent = team,
                 score = strsplit(score, split = '-') %>%
                     lapply(., rev) %>%
                     sapply(., paste, collapse = '-'),
                 line = -1 * line,
                 location = location)]

lines <- rbind(data, lines.dup)

## setkey(lines, team, season, date)
## lines[, nhours.lgame   := c(NA, diff(date)),              by = list(team, season)]
## lines[, last.game.loc  := c(NA, lag(location)[1:.N-1]),   by = list(team, season)]
## lines[, last.game.time := c(NA, lag(hour(date))[1:.N-1]), by = list(team, season)]
