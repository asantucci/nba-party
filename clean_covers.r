################################################################################
################################################################################
################################################################################
###
### Title: Cleaning overs Data
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: 'raw_data/covers/[year]-[month]-[day].csv'
###
### Dependencies: data.table, magrittr
###
################################################################################
################################################################################

##################################################
### Set up Workspace, Read in Raw Data
##################################################

require(data.table)
require(magrittr)

files <- list.files('raw_data/covers', full.names = T)
lines <- lapply(files, fread) %>% rbindlist

##################################################
### Clean up date and line variables. Remove extraneous vars.
##################################################

lines[, game.time := gsub('^.* ([0-9:APM]+)$', '\\1', date)]
lines[, date := as.Date(date, format = '%A, %B %d %Y')]

lines[, line := gsub('^(-?[0-9.]+)/.*$', '\\1', line)]
lines[, line.ts := NULL]
lines[, ou := NULL]
lines[, game.time := NULL]


##################################################
### Expand team abbreviations.
##################################################

lines[, `:=`(home = tolower(home), away = tolower(away))]

abbrs <- unique(lines$home) %>% sort
fulls <- c('Atlanta Hawks',   'Brooklyn Nets',          'Boston Celtics',      'Charlotte Bobcats',
           'Chicago Bulls',   'Cleveland Cavaliers',    'Dallas Mavericks',    'Denver Nuggets',
           'Detroit Pistons', 'Golden State Warriors',  'Houston Rockets',     'Indiana Pacers',
           'LA Clippers',     'LA Lakers',              'Memphis Grizzlies',   'Miami Heat',
           'Milwaukee Bucks', 'Minnesota Timberwolves', 'New Orleans Hornets', 'New York Knicks',
           'Oklahoma City Thunder',  'Orlando Magic',   'Philadelphia Sixers', 'Phoenix Suns',
           'Portland Trail Blazers', 'San Antonio Spurs', 'Sacramento Kings',
           'Toronto Raptors',        'Utah Jazz',         'Washington Wizards')

teams <- data.frame(team = fulls %>% tolower, abbr = abbrs %>% tolower)
lines <- merge(lines, teams, by.x = 'home', by.y = 'abbr', all.x = T)
setnames(lines, 'team', 'home.team')
lines <- merge(lines, teams, by.x = 'away', by.y = 'abbr', all.x = T)
setnames(lines, 'team', 'away.team')

##################################################
### Remove missing observations. Set line as numeric.
##################################################

lines <- lines[line != 'OFF', list(date, home.team, away.team, home.pts, away.pts, line)]
lines[, line := as.numeric(line)]
save(lines, file = 'tmp_data/covers_lines.RData')
