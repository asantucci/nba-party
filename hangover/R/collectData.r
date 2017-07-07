#' Defunct: used to parse data from gold-sheet.com.
#'
#' Fetches data for a particular team from raw data from gold-sheet.com
#' @param team A string describing a team name.
#' @param beg A starting index of relevant data.
#' @param end An ending index of relevant data.
#' @param data A dataframe from which to subset.
#' @keywords Fetch, gold-sheet.
#' @export
#' @examples
#' # not run: Fetch('los angeles lakers', 10, 20, data)
Fetch <- function(team, beg, end, data) {
    datum <- data[beg:end]
    ### We have to be careful for data entry errors. Ex: missing delimiter.
    datum <- gsub("W\\+12 89-98", "W  +12  89-98", datum) %>%               # Separate fields.
        gsub("([0-9]{3}[OU][[:blank:]]+)(.*)", "\\1", .) %>% # Throw out playoff game (not formatted)
        strsplit('[[:blank:]]{2,}')
    datum <- Filter(function(x) !any(grepl("CAN\\.", x)), datum)
    datum <- do.call(rbind, args = datum) %>%
        data.table(.) %>% cbind(team)
}

#' Retrieve data from a raw gold-sheet.com datafile.
#'
#' Given a raw datafile from gold-sheet.com, this function parses the data.
#' @param datafile A string describing input file.
#' @keywords collect, legacy
#' @export
#' @examples
#' # not run: collectData('raw_data/2011.txt')
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
    data <- mapply(Fetch, team = teams$team, beg = teams$beg, end = teams$end,
                   MoreArgs = list(data=data), SIMPLIFY = F)
    data <- rbindlist(data)
    data[['season']] = gsub('raw_data/([0-9_]{9})\\.txt', '\\1', datafile)
    setnames(data, c('date', 'opponent', 'outcome', 'spread', 'score',
                     'location', 'ou', 'team', 'season'))
    setcolorder(data, c('season', 'date', 'team', 'opponent', 'outcome', 'spread', 'score',
                        'location', 'ou'))
    return(data)
}
