#' Place a bet on yet-unseen data.
#'
#' This function compares our prediction with the actual outcome of the game.
#' We allow the house bookmaker a $10 cut for each bet placed, regardless of outcome.
#' @param our.prediction A fitted label (0 or 1).
#' @param actual.outcome A true label (0 or 1).
#' @param odds Bookkeeper's odds of success, i.e. of actual outcome == 1.
#' @keywords Bet
#' @export
#' @examples
#' Bet(1, 0, 0.55)
Bet <- function(our.prediction, actual.outcome, odds) {
    actual.outcome = as.integer(actual.outcome)
    if (is.na(our.prediction)) return(NA_integer_)
    if (our.prediction == actual.outcome) {
        if (our.prediction > 0)
            return(100 / odds - 100 - 10)
        else if (our.prediction == 0)
            return(100 / (1-odds) - 100 - 10)
    } else if (our.prediction != actual.outcome)
        return(-100)
}
