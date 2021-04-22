#' Download catalogue number worth of ABS time series data if it has been
#' updated since the version stored locally
#'
#' Checks the latest observation date in a local ABS time series file; compares
#' that to the latest date for the catalogue number on the ABS website. If the
#' remote version is newer than the local version, download the catalogue
#' number using `readabs::read_abs()`
#'
#' @param cat_no character, such as "6202.0" or "6345.0"
#' @param path default is `file.path("data-raw", "abs-ts")`
#' @param ... arguments passed to `readabs::read_abs()`
#'
#' @examples
#' \dontrun{
#' read_abs_if_updated("6345.0")
#' }
#' @export

read_abs_if_updated <- function(cat_no = NULL,
                                path = file.path("data-raw", "abs-ts"),
                                ...) {

  safely_read_local <- purrr::safely(readabs::read_abs_local)

  local_result <- safely_read_local(cat_no = cat_no,
                                    path = path)

  local_exists <- if(!is.null(local_result$result)) {
    TRUE
  } else {
    FALSE
  }

  if (isFALSE(local_exists)) {
    df <- readabs::read_abs(cat_no = cat_no,
                            path = path,
                            check_local = FALSE,
                            ...)
  } else {
    local_df <- local_result$result

    max_local_date <- max(local_df$date)

    latest_date <- readabs::check_latest_date(cat_no = cat_no)

    if (latest_date > max_local_date) {
      df <- readabs::read_abs(cat_no = cat_no,
                              path = path,
                              check_local = FALSE,
                              ...)
    } else {
      df <- local_df
    }

  }

  unlink(file.path(path, cat_no), recursive = TRUE)

  return(df)
}
