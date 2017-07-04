#' Calculate distances between locations. Save to path.
#'
#' Given a listing of locations (including longitude and latitude) and a save.path,
#' this function calculates pairwise distances and converts output to miles.
#' @param locations A data.table describing location longitude and latitude.
#' @param save.path A string describing output file location.
#' @param team.names A character vector describing team-names, for labelling of output matrix.
#' @param kmToMiles A magic number: the number of kilometers in a mile.
#' @keywords calculate, distances
#' @export
#' @examples
#'
calculateDistances <- function(locations, save.path, team.names, kmToMiles = 1.60934) {
    dmat <- spDists(locations[, c('lon', 'lat')] %>% as.matrix, longlat = T)
    dmat <- dmat / kmToMiles
    rownames(dmat) <- team.names
    colnames(dmat) <- team.names
    save(dmat, file = save.path)
    return(NULL)
}
