
Fetch <- function(team, beg, end, data, year) {
    datum <- data[beg:end]
    ### We have to be careful for data entry errors. Ex: missing delimiter.
    datum <- gsub("W\\+12 89-98", "W  +12  89-98", datum) %>%               # Separate fields.
        gsub("([0-9]{3}[OU][[:blank:]]+)(.*)", "\\1", .) %>% # Throw out playoff game not formatted nicely.
        strsplit('[[:blank:]]{2,}')
    datum <- Filter(function(x) !any(grepl("CAN\\.", x)), datum)
    datum <- do.call(rbind, args = datum) %>%
        data.table(.) %>% cbind(team)
}


collectData <- function(datafile) {
    data <- readLines(datafile) # 'raw_data/2011.txt')

    ### Remove blank lines.
    blanks <- grep("^[[:blank:]]*$", data)
    data <- data[-blanks]

    ### Find team names, aka starting indices. 
    beg <- grep("^\\(SUR", data) - 1     # Look one row before team-summary for team-name.
    end <- c(beg[-1] - 1, length(data))  # Find limits of data for each team.
    teams <- data[beg]

    ### This data frame describes where data start-and-stop for each team.
    teams <- data.frame(team = teams, beg = beg+2, end = end)

    data <- mapply(Fetch, team = teams$team, beg = teams$beg, end = teams$end, year = '2011',
                   MoreArgs = list(data=data), SIMPLIFY = F)

    data <- rbindlist(data)
    data[['season']] = gsub('raw_data/([0-9_]{9})\\.txt', '\\1', datafile)

    setnames(data, c('date', 'opponent', 'outcome', 'spread', 'score', 'location', 'ou', 'team', 'season'))
    setcolorder(data, c('season', 'date', 'team', 'opponent', 'outcome', 'spread', 'score', 'location', 'ou'))
    return(data)
}

getFullTeamName <- function(needle, haystack)
    grep(needle, haystack, value = T)
