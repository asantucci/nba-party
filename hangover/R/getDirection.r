#' Lookup direction of travel between two locations.
#'
#' Given two locations and a (character) matrix describing direction, retrieve direction of travel from last location to current location.
#' @param last.loc A string or integer describing a row index.
#' @param cur.loc A string or integer describing a column index.
#' @param dirs A character matrix describing direction of travel between locations.
#' @keywords get, direction
#' @export
#' @examples
#'
getDirection <- function(last.loc, cur.loc, dirs)
    if(!is.na(last.loc) && !is.na(cur.loc))
        dirs[last.loc, cur.loc]
