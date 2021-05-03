#' Filter a dataframe based on series ID
#'
#' Series ID found using `find_lfs_series()`
#'
#' @param df Data frame, presumed to contain a `series_id` column
#' containing ABS Time Series IDs. Default is `dash_data`, the
#' data frame used by {djprlabourdash}.
#' @param indicator Indicator name, such as 'unemployment rate'.
#' See `?find_lfs_series()` for possible values.
#' @param series_type Default is 'seasonally adjusted'.
#' @param ... arguments passed to `find_lfs_series()` and used to
#' filter `df`.
#' @return a data frame (tibble) filtered based on the filtering
#' conditions specified in `...`
#' @examples
#' \dontrun{
#' abs_6202 <- readabs::read_abs("6202.0", 5)
#' filter_lfs_data(abs_6202, "unemployment rate", state = "victoria")
#' }
#' @export

filter_lfs_data <- function(df = dash_data,
                            indicator,
                            series_type = "seasonally adjusted",
                            ...) {

  matching_ids <- find_lfs_series(indicator = indicator,
                                  series_type = series_type,
                                  ...)

  if (length(matching_ids) == 0) {
    stop("Could not find time series IDs that match your filters.")
  }

  df %>%
    dplyr::filter(.data$series_id %in% .env$matching_ids)
}
