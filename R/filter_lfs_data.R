#' Filter a dataframe based on series ID
#'
#' Series ID found using `find_lfs_series()`
#'
#' @param indicator Indicator name, such as 'unemployment rate'.
#' See `?find_lfs_series()` for possible values.
#' @param series_type Default is 'seasonally adjusted'.
#' @param ... arguments passed to `find_lfs_series()` and used to
#' filter `df`.
#' @param df Data frame, presumed to be nested, and to
#' contain a `series_id` column with ABS Time Series IDs and
#' a `data` column that is nested. Default is `dash_data`, the
#' data frame used by `{djprlabourdash}`.
#' @return a data frame (tibble) filtered based on the filtering
#' conditions specified in `...`
#' @examples
#' \dontrun{
#' abs_6202 <- readabs::read_abs("6202.0", 5)
#' filter_lfs_data("unemployment rate", state = "victoria", df = abs_6202)
#' }
#' @export

filter_lfs_data <- function(indicator,
                            series_type = "seasonally adjusted",
                            ...,
                            df = dash_data) {

  matching_ids <- find_lfs_series(indicator = indicator,
                                  series_type = series_type,
                                  ...)

  if (length(matching_ids) == 0) {
    stop("Could not find time series IDs that match your filters.")
  }

  df %>%
    dplyr::filter(.data$series_id %in% .env$matching_ids) %>%
    tidyr::unnest(cols = data) %>%
    dplyr::ungroup()
}
