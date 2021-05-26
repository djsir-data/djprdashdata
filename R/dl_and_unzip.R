#' Download a ZIP file from the ABS and unzip the contents
#'
#' @param catalogue_string Such as "labour-force-australia" or "labour-force-australia-detailed".
#' See `?readabs::search_files()` for more.
#' @param include_pivots when `FALSE`, pivot tables aren't downloaded
#' @param dest_dir Path to directory to which ZIP contents should be extracted
#'
#' @examples
#' \dontrun{
#' dl_and_unzip("labour-force-australia")
#' }
#'
dl_and_unzip <- function(catalogue_string,
                         include_pivots = FALSE,
                         dest_dir = file.path("data-raw", "raw-data", catalogue_string)) {
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir))

  # Make sure to reset user's timeout option when function exits
  user_timeout <- getOption("timeout")
  on.exit(options("timeout" = user_timeout))
  # Set per-file timeout in seconds
  options("timeout" = 5 * 60)

  # Get fragments of filenames to download
  file_basenames <- readabs::search_files(
    string = ".zip",
    catalogue = catalogue_string
  ) %>%
    basename()

  if (isFALSE(include_pivots)) {
    file_basenames <- file_basenames[!grepl("pivot", file_basenames)]
  }

  # Download files to temp_dir
  files <- purrr::map_chr(
    .x = file_basenames,
    .f = readabs::download_abs_data_cube,
    catalogue_string = catalogue_string,
    path = temp_dir
  )

  is_pivot <- grepl("pivot", file_basenames, ignore.case = TRUE)
  is_pivot <- ifelse(is_pivot, "pivot", "time-series")

  dest_dirs <- file.path(dest_dir, is_pivot)

  purrr::walk2(
    .x = files,
    .y = dest_dirs,
    .f = ~ zip::unzip(
      zipfile = .x,
      overwrite = TRUE,
      junkpaths = FALSE,
      exdir = .y
    )
  )

  invisible(return(dest_dir))
}

#' Download files with dl_and_unzip() and read using read_abs_local()
#' Downloads a zip to a temp file, unzips the contents to `dest_dir`,
#' loads the contents using `readabs::read_abs_local_dir()`
#' @param catalogue_string eg. "labour-force-australia"
#' @param dest_dir Directory in which
dl_and_read <- function(catalogue_string,
                        dest_dir = file.path("data-raw", "raw-data", catalogue_string)) {

  dl_and_unzip(catalogue_string = catalogue_string,
               include_pivots = FALSE,
               dest_dir = dest_dir)

  ts_dir <- here::here(dest_dir, "time-series")
  print("Loading files from ", )

  read_abs_local_dir(ts_dir)

}
