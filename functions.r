
SubsetBLS <- function(year, party.regex, sport,
                      raw.path = 'raw_data/bls/', save.path = 'tmp_data/bls/') {
    ### Create a listing of files for all relevant counties, load data.
    foldr <- list.files(path = raw.path, pattern = paste0(year, '.q1'), full.names = T)
    files <- list.files(path = foldr,
                        pattern = '(county,)|(district of columbia)|(parish)',
                        full.names = T, ignore.case = T)
    bls <- lapply(files, fread, colClasses = 'character',
                  select = c('area_fips', 'year', 'qtr', 'industry_code', 'area_title',
                             'industry_title', 'agglvl_title', 'size_title',
                             'qtrly_estabs_count')) %>% rbindlist
    setnames(bls, gsub('_', '.', names(bls)))
    ### We will take our own total later, so for now we take granular data.
    bls <- bls[grep("County, NAICS 6-digit", agglvl.title)]
    ### Create regex to subset to drinking establshimensts.
    party.regex <- paste0('(', party.regex, ')', collapse = '|')
    bls <- bls[grep(party.regex, industry.title, ignore.case = T)]
    write.csv(x = bls, file = paste0(save.path, year, '_', sport, '.csv'), row.names = F)
    rm(bls)
    gc()
    return(NULL)
}


Predict <- function(prediction, vegas, threshold) {
    if (is.na(prediction)) return(NA)
    dif <- prediction - vegas
    if (abs(dif) > threshold) {
        if (dif < 0)
            return(0)
        else
            return(1)
    }
    return(NA)
}

Bet <- function(our.prediction, actual.outcome, odds) {
    actual.outcome = as.integer(actual.outcome)
    if (is.na(our.prediction)) return(NA_integer_)
    if (our.prediction == actual.outcome) {
        if (our.prediction > 0)
            return(100 / odds - 100 - 5)
        else if (our.prediction == 0)
            return(100 / (1-odds) - 100 - 5)
    } else if (our.prediction != actual.outcome)
        return(-100)
}



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
