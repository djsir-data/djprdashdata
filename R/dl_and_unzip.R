#' Download a ZIP file from the ABS and unzip the contents
#'
#' @param catalogue_string Such as "labour-force-australia" or "labour-force-australia-detailed".
#' See `?readabs::search_files()` for more.
#' @param dest_dir Path to directory to which ZIP contents should be extracted
#'
#' @examples
#' \dontrun{
#' dl_and_unzip("labour-force-australia")
#' }
#'
dl_and_unzip <- function(catalogue_string,
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

  return(TRUE)
}
