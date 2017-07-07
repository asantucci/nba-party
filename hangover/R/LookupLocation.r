#' Subset data to observations satisfying regex containing location and state.
#'
#' We can't simply merge. We have to look for the county within the MSA.
#' For each location, we fetch a panel of corresponding musicians data, 2010-2016.
#' @param loc A string describing location to search for.
#' @param st A string describing the state to search for.
#' @param data A data.frame containing the observations we wish to extract.
#' @param team A string to be used as a feature describing team.
#' @keywords lookup, location
#' @export
#' @examples
LookupLocation <- function(loc, st, data, team = NULL) {
    sb <- data[grepl(loc, area.title, ignore.case = T) & grepl(st,  area.title)]
    sb[, `:=`(locality = loc, state = st)]
    if (!is.null(team)) sb[, team := team]
    return(sb)
}
