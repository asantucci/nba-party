################################################################################
################################################################################
################################################################################
###
### Title: Scraping Lines Data from Covers.com
###
### Andreas Santucci
###
### Date: March 2017
###
### Inputs: NA
###
### Dependencies: data.table, XML
###
################################################################################
################################################################################

##############################
### Set up Workspace
##############################
require(data.table)
require(XML)

load(file = 'tmp_data/game_days.RData')
game.days <- Filter(function(x) grepl("^201[^0]", x), game.days) 
game.days <- gsub('-0([0-9])-', '-\\1-', game.days)

##############################
### Define Functions
##############################
scrapeGame <- function(link) {
    tryCatch(expr = {
        text <- readLines(link, warn = F)
        ### We manually extract the date-field from HTML text.
        date <- grep("\\<Date[^(]", text, value = T) %>% gsub('^.*- (.*)<br>', '\\1', .)
        ### Determine the event-id from the link, such that we may scrape-lines.
        event.id <- gsub('^.*boxscore([0-9]+).html$', '\\1', link)
        lines <- paste0('http://www.covers.com/odds/linehistory.aspx?eventId=',
                        event.id, '&sport=NBA')
        ### Scrape lines, just grabbing one entry for now.
        lines <- readHTMLTable(lines, header = T, which = 1)[1, ]  # <-- One entry assumption.
        data  <- data.frame(date = date, lines)
        setnames(data, c('date', 'line.ts', 'line', 'ou'))
        return(data)
    }, error = function(e) {
        Sys.sleep(1)
        cat(paste0('Error when downloading from: ', link, '...trying again\n'))
        scrapeGame(link)
    })
}

scrapeCoverLines <- function(date, rescrape = F, save.path = 'raw_data/covers/') {
    if (!rescrape && file.exists(paste0(save.path, date, '.csv')))
        return(NULL)
    cat(paste0("Now trying to download covers for date: ", date, "\n"))
    ### Bring up a listing of games played on a particular day.
    tryCatch(expr = {
        pg <- paste0('http://www.covers.com/sports/NBA/matchups?selectedDate=', date)
        doc <- htmlParse(pg)
        ### Each entry in games.played is a link to a detailed score-report.
        games.played <- xpathSApply(doc, "//a/@href") %>%
            grep('boxscore', ., value = T)
        ### Games.stats contains quarter-by-quarter scores, alongside team-names.
        games.stats <- readHTMLTable(doc)
        teams <- t(sapply(games.stats, function(x) x[[1]] %>% as.character)) # Grab team names.
        score <- t(sapply(games.stats, function(x) x[[ncol(x)-1]]))  # Grab column of points totals.
        colnames(teams) <- c('away', 'home')
        colnames(score) <- c('away.pts', 'home.pts')
        rownames(teams) <- NULL
        ### For each game played, scrape a listing of lines.
        lines <- lapply(games.played, scrapeGame) %>% rbindlist
        data <- cbind(teams, score, lines)
        write.csv(data, file = paste0(save.path, date, '.csv'), row.names = F)
        return(NULL)
    }, error = function(e) {
        Sys.sleep(1)
        cat(paste0('Error when downloading data for date: ', date, '\n'))
        scrapeCoverLines(date)
    })
}

##############################
### Example
##############################
require(parallel)
cl <- makeCluster(detectCores())
clusterCall(cl, function() {
    require(data.table)
    require(magrittr)
    require(XML)
})

clusterExport(cl, 'scrapeGame')

lines <- parLapplyLB(cl, game.days, scrapeCoverLines)
## lines <- lapply(game.days, scrapeCoverLines)
## save(lines, file = 'raw_data/covers.RData')
lines <- rbindlist(lines)

### Second pass?
game.days <- as.Date(game.days)
game.days <- setdiff(game.days %>% as.character, unique(lines$date) %>% as.character)
game.days <- gsub('-0([0-9])-', '-\\1-', game.days)
second.pass <- lapply(game.days, scrapeCoverLines)
second.pass2 <- rbindlist(second.pass)
save(second.pass2, file = 'tmp_data/second_pass.RData')
lines <- rbind(lines, second.pass2)

lines <- lines[, lapply(.SD, as.character), .SDcols = 1:ncol(lines)]
save(lines, file = 'raw_data/covers.RData')

lines[, game.time := gsub('^.* ([0-9:APM]+)$', '\\1', date)]
lines[, date := as.Date(date, format = '%A, %B %d %Y')]

lines[, line := gsub('^(-?[0-9.]+)/.*$', '\\1', line)]
lines[, line.ts := NULL]
lines[, ou := NULL]
lines[, game.time := NULL]

lines[, `:=`(home = tolower(home), away = tolower(away))]

abbrs <- unique(lines$home) %>% sort
fulls <- read.table(text = 'Atlanta Hawks
Brooklyn Nets
Boston Celtics
Charlotte Bobcats
Chicago Bulls
Cleveland Cavaliers
Dallas Mavericks
Denver Nuggets
Detroit Pistons
Golden State Warriors
Houston Rockets
Indiana Pacers
LA Clippers
LA Lakers
Memphis Grizzlies
Miami Heat
Milwaukee Bucks
Minnesota Timberwolves
New Orleans Hornets
New York Knicks
Oklahoma City Thunder
Orlando Magic
Philadelphia Sixers
Phoenix Suns
Portland Trail Blazers
San Antonio Spurs
Sacramento Kings
Toronto Raptors
Utah Jazz
Washington Wizards', sep = '\n', stringsAsFactors = F)

teams <- data.frame(team = fulls[[1]] %>% tolower, abbr = abbrs %>% tolower)
lines <- merge(lines, teams, by.x = 'home', by.y = 'abbr', all.x = T)
setnames(lines, 'team', 'home.team')
lines <- merge(lines, teams, by.x = 'away', by.y = 'abbr', all.x = T)
setnames(lines, 'team', 'away.team')

lines <- lines[line != 'OFF', list(date, home.team, away.team, home.pts, away.pts, line)]
lines[, line := as.numeric(line)]
save(lines, file = 'tmp_data/covers_lines.RData')

