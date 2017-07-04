#' Lookup distance between two locations.
#'
#' Given two locations and a distance matrix, perform a table-lookup.
#' @param current A string describing a row name or an integer describing row-index.
#' @param last A string describing a column name or an integer describing column-index.
#' @param distances A matrix containing distances between pairwise points.
#' @keywords get, distance
#' @export
#' @examples
#' teams <- c('los angeles lakers', 'new york knicks')
#' mat <- matrix(data = c(0, 999, 999, 0), nrow = 2, byrow = T)
#' rownames(mat) <- teams
#' colnames(mat) <- teams
#' getDist('los angeles lakers', 'new york knicks', mat)
getDist <- function(current, last, distances)
    if (!is.na(current) && !is.na(last))
        distances[current, last]
