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
#' @param include_trend logical; default is `FALSE`. Should 'trend' data be
#' included in the data frame?
#' @param ... arguments passed to `readabs::read_abs()`
#'
#' @examples
#' \dontrun{
#' read_abs_if_updated("6345.0")
#' }
#' @export
#' @importFrom rlang .data .env


read_abs_if_updated <- function(cat_no = NULL,
                                path = here::here("data-raw", "abs-ts"),
                                include_trend = FALSE,
                                ...) {

  temp_dir <- tempdir()
  on.exit(unlink(temp_dir))

  qs_file <- here::here(path,
                        paste0(gsub("\\.", "-", cat_no), ".qs"))

  if (!dir.exists(path)) {
    dir.create(path = path, recursive = TRUE)
  }

  safely_read_local <- purrr::safely(qs::qread)


  local_result <- safely_read_local(file = qs_file)

  local_exists <- if(!is.null(local_result$result)) {
    TRUE
  } else {
    FALSE
  }

  if (isFALSE(local_exists)) {
    # No local file, download from ABS
    read_abs_and_save(cat_no = cat_no,
                      temp_path = temp_dir,
                      qs_file = qs_file,
                      include_trend = include_trend)

  } else {
    # Load local file, check if it's up to date
    local_df <- local_result$result

    local_df <- local_df %>%
      tidyr::unnest(cols = .data$data)

    max_local_date <- max(local_df$date)

    latest_date <- readabs::check_latest_date(cat_no = cat_no)

    if (latest_date > max_local_date) {
      # Local file isn't up-to-date, so get a new one from ABS
      read_abs_and_save(cat_no = cat_no,
                        temp_path = temp_dir,
                        qs_file = qs_file,
                        include_trend = include_trend)

    } else {
      df <- local_df
    }

  }

  return(df)
}

#' Download a catalogue number worth of ABS time series data; remove some
#' unneeded columns, then save as a qs file
#' @noRd
#' @keywords internal

read_abs_and_save <- function(cat_no,
                              temp_path,
                              qs_file,
                              include_trend = FALSE,
                              ...) {

  df <- readabs::read_abs(cat_no = cat_no,
                          path = temp_path,
                          check_local = FALSE,
                          ...)

  df <- df %>%
    dplyr::select(-.data$sheet_no,
                  -.data$table_title,
                  -.data$collection_month)

  if (isFALSE(include_trend)) {
    df <- df %>%
      dplyr::filter(.data$series_type != "Trend")
  }

  compress_and_save_df(df = df, qs_file = qs_file)
}
