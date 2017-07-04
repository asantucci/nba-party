#' Create a panel-data set from lines data.
#'
#' Given a dataframe of game-outcomes, we create a panel dataset.
#' In particular, for each game we want two observations.
#' The observations will be different since the lagged variables will be different.
#' I.e. the number of days since last game and distance traveled will be different for each
#' of the teams. To do this, we simply 'reverse' each obseration, carefully.
#' @param dt A data.table containing game-outcomes.
#' @param sport A string, one of 'nba' or 'mlb'
#' @keywords create, panel
#' @export
#' @examples
#'
createPanelDataset <- function(dt, sport) {
    cp <- copy(dt)
    setnames(cp, c('team', 'opponent'), c('opponent', 'team'))
    set(cp, j = 'score', value = strsplit(cp$score, split = '-') %>%
                             lapply(., rev) %>%
                             sapply(., paste, collapse = '-'))
    if (sport == 'nba')      set(cp, j = 'line', value = -1 * cp$line)
    else if (sport == 'mlb') set(cp, j = 'odds', value =  1 - cp$odds)
    if (sport == 'nba')      set(cp, j = 'outcome', value = ifelse(cp$outcome == 'L', 'W', 'L'))
    rbind(dt, cp)
}
