#' Converts from degrees to a compass direction.
#'
#' Source: https://stackoverflow.com/questions/7490660/converting-wind-direction-in-angles-to-text-words
#' @param x A double representing directional degrees.
#' @keywords degrees, compass, direction
#' @export
#' @examples
#' degToCompass(300)
degToCompass <- function(x) {
    val <- ((x/22.5)+.5)
    arr <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
    arr[(val %% 16)]
}
