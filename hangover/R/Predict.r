#' Label a fitted probability as 0, 1, or NA.
#'
#' A labeling function which outputs a value if and only if the prediction
#' differs sufficiently from the bookmakers expectations.
#' @param prediction A fitted probability.
#' @param vegas Bookkeeper's odds.
#' @param threshold A tuning parametr: we only place a bet if we differ from bookkeeper's expectations by `threshold` amount.
#' @keywords predict
#' @export
#' @examples
#' Predict(0.6, 0.5, 0.05)
Predict <- function(prediction, vegas, threshold) {
    if (is.na(prediction)) return(NA)
    dif <- prediction - vegas
    if (abs(dif) > threshold) {
        if (dif < 0)  return(0)
        else          return(1)
    }
    return(NA)
}
