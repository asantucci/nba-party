#' Get lon/lat for a listing of teams.
#'
#' Geocode team locations to get lat-lon and addresses.
#' @param teams A character vector of team-names we wish to geocode.
#' @keywords get, locations
#' @export
#' @examples
#'
getLocations <- function(teams) {
    locs <- sapply(teams, geocode, simplify = F) %>% do.call(rbind, .)
    locs[['team']] <- rownames(locs)
    setDT(locs)
    return(locs)
}
