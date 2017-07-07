#' A function to geocode team-locations.
#'
#' This function takes as input a listing of team-names for which we
#' retrieve location data.
#' @param teams A vector of strings, each a team-name.
#' @param sport A string; one of 'nba' or 'mlb'.
#' @keywords geocode, ggmap
#' @export
#' @examples
#' # not run: MyGeoCode('Los Angeles Lakers', 'nba')
MyGeoCode <- function(teams, sport) {
    locs <- lapply(teams, geocode, output = 'more') %>% rbindlist(., fill = T)
    setnames(locs, gsub('_', '.', names(locs)))
    locs <- locs[, lapply(.SD, as.character), .SDcols = 1:ncol(locs)]
    ### We take care to fetch the MSA for each area (i.e. locality)
    ### Sometimes, we need to make manual adjustments. We do this for NBA and MLB.
    if (sport == 'nba')  {
        locs[is.na(locality), locality := 'New York']
        locs[locality == 'El Segundo', locality := 'Los Angeles']
    }
    locs <- data.table(team = teams, locality = locs$locality,
                       state = locs$administrative.area.level.1,
                       lon = locs$lon %>% as.numeric, lat = locs$lat %>% as.numeric)
    if (sport == 'mlb') {
        locs[is.na(locality) & state == 'Florida', `:=`(locality = 'Atlanta', state = 'Georgia')]
        locs[is.na(locality) & state == 'New York', locality := 'New York']
        locs[locality == 'Broomfield' & state == 'Colorado', locality := 'Denver']
        locs[team == 'texas rangers', locality := 'Dallas']
    }
    return(locs)
}
