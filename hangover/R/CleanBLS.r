#' A function to clean BLS data.
#'
#' Given a string describing what kinds of business patterns to look for, a labeling suffix,
#' a sport, and an aggregation function, this functional subsets each year of raw BLS
#' data such that only observations matching the party.regex are retained.
#' The data are then aggregated using the aggregation function FUN.
#' Results are saved to disk according to a labeling which uses suffix and sport.
#' @param party.regex A character vector of patterns to search for in our BLS data.
#' @param suffix A labeling suffix to be used when saving the output file.
#' @param sport A string, one of 'nba' or 'mlb'
#' @param FUN An aggregation function to be used in collapsing BLS data by locality.
#' @param years A vector of integers describing which years to collect data for.
#' @keywords BLS, clean
#' @export
#' @examples
#' # not run: CleanBLS(c('recording studio', 'musical group'), 'DJ_proxy', 'nba', sum)
CleanBLS <- function(party.regex, suffix, sport, FUN, years = 2010:2016) {
    ### Subset each year of BLS data to only observations matching regular expression.
    lapply(years, hangover::SubsetBLS, party.regex = party.regex, suffix = suffix)
    files <- list.files(path = 'tmp_data/bls',
                        pattern = paste0(suffix, '\\.csv$'), full.names = T)
    bls <- lapply(files, fread) %>% rbindlist
    dt <- bls[, list(variable = FUN(qtrly.estabs.count %>% as.numeric)),
              by = list(area.title, year)]
    dt[, year := as.integer(year)]
    save(dt, file = paste0('tmp_data/', suffix, '.RData'))
}
