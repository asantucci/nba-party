#' A function to subset BLS data into manageable chunks.
#'
#' Subset BLS data into manageable chunks, keeping only observations we care about.
#' @param year An integer describing which year of BLS data to subset.
#' @param party.regex A character vector describing patterns to look for in BLS data.
#' @param sport A string, one of 'nba' or 'mlb'.
#' @param suffix A string describing how to prefix the output file.
#' @param raw.path A string describing the input file path. Defaults to 'raw_data/bls/'.
#' @param save.path A string describing the output file path. Defaults to 'tmp_data/bls/'.
#' @keywords subset, bls
#' @export
#' @examples
#' SubsetBLS(2010, 'sound recording', 'nba', 'sound_recording_studios')
SubsetBLS <- function(year, party.regex, sport, suffix,
                      raw.path = 'raw_data/bls/', save.path = 'tmp_data/bls/') {
    ### Create a listing of files for all relevant MSA's, and load data.
    foldr <- list.files(path = raw.path, pattern = paste0(year, '.q1'), full.names = T)
    files <- list.files(path = foldr,
                        pattern = 'm(icro)?sa\\.csv',
                        full.names = T, ignore.case = T)
    bls <- lapply(files, fread, colClasses = 'character',
                  select = c('area_fips', 'year', 'qtr', 'industry_code',
                             'area_title', 'industry_title', 'agglvl_title',
                             'size_title', 'qtrly_estabs_count')) %>% rbindlist
    setnames(bls, gsub('_', '.', names(bls)))
    ### We will take our own total later, so for now we take granular data.
    bls <- bls[grep("NAICS 6-digit", agglvl.title)]
    ### Create regex to subset to drinking establshimensts.
    party.regex <- paste0('(', party.regex, ')', collapse = '|')
    bls <- bls[grep(party.regex, industry.title, ignore.case = T)]
    write.csv(x = bls, file = paste0(save.path, year, '_', sport, '_', suffix, '.csv'),
              row.names = F)
    rm(bls)
    gc()
    return(NULL)
}
