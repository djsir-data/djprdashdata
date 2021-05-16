
#' Get all pivot tables for which functions have been created
#' Need to manually add them
#'
get_all_lfs_pivots <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                               all_states = FALSE) {
  lfs_eq03 <- get_lfs_eq03(
    path = path,
    all_states = all_states
  )

  lfs_eq03
}

#' Download and tidy data cube EQ03 from detailed Labour Force
#' @noRd
get_lfs_eq03 <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                         all_states = FALSE) {
  raw_pivot <- get_lfs_pivot(cube = "EQ03")

  names(raw_pivot) <- c(
    "date",
    "sex",
    "gcc_restofstate",
    "industry",
    "Employed full-time",
    "Employed part-time",
    "Number of hours actually worked in all jobs (employed full-time)",
    "Number of hours actually worked in all jobs (employed part-time)"
  )

  tidy_pivot <- raw_pivot %>%
    tidyr::pivot_longer(
      cols = !dplyr::one_of(c(
        "date",
        "sex",
        "gcc_restofstate",
        "industry"
      )),
      names_to = "indicator",
      values_to = "value"
    )

  # Create series IDs
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_id = paste(.data$sex,
        .data$gcc_restofstate,
        .data$industry,
        .data$indicator,
        sep = "_"
      ) %>% tolower(),
      series = paste(.data$indicator,
        .data$gcc_restofstate,
        .data$sex,
        .data$industry,
        sep = " ; "
      ),
      series_type = "Original",
      table_no = "EQ03",
      data_type = "STOCK",
      frequency = "Month",
      unit = "000",
      cat_no = "6291.0.55.001"
    )

  if (isFALSE(all_states)) {
    tidy_pivot <- tidy_pivot %>%
      dplyr::filter(.data$gcc_restofstate %in% c(
        "Greater Melbourne",
        "Rest of Vic."
      ))
  }

  tidy_pivot
}

#' Download and tidy ABS pivot cubes
#'
#' @export
#' @param cube Unique fragment of datacube filename, such as "EQ03"
#' @param catalogue_string Such as "labour-force-australia-detailed". See
#' `?readabs::show_available_catalogues` and `?readabs::search_catalogues`.
#' @param path Location to store the raw data cube
#' @param sheet Name of the Excel sheet containing the data for the pivot cube
#' @param col_names Passed to `col_names` argument of `readxl::read_excel()`.
#' If `TRUE`, the default, the text in the spreadsheet will be used as the
#' column names; a character vector can be supplied instead.
#' @examples
#' \dontrun{
#' get_lfs_pivot("EQ03")
#' }
#'
get_lfs_pivot <- function(cube,
                          catalogue_string = "labour-force-australia-detailed",
                          path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                          sheet = "Data 1",
                          col_names = TRUE) {
  file <- readabs::download_abs_data_cube(
    catalogue_string = catalogue_string,
    cube = cube,
    path = path
  )

  tidy_lfs_pivot(
    path = file,
    sheet = sheet,
    col_names = col_names
  )
}

tidy_lfs_pivot <- function(path,
                           sheet,
                           col_names) {
  skip <- 3

  if (!isTRUE(col_names)) {
    skip <- skip + 1
  }

  df <- readxl::read_excel(
    path = path,
    sheet = sheet,
    col_names = col_names,
    skip = 3
  ) %>%
    dplyr::mutate(dplyr::across(where(~inherits(.x, "POSIXct")),
                                as.Date))

  df
}
