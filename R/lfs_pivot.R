
#' Get all pivot tables for which functions have been created
#' Need to manually add them
#' @param path Path to directory in which to store downloaded file(s)
#' @param all_states logical, `FALSE` by default. When `FALSE`, states other than
#' Victoria will be removed.
#'
#'
get_tidy_lfs_pivots <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                                all_states = FALSE) {
  lfs_eq03 <- get_lfs_eq03(
    path = path,
    all_states = all_states
  )

  lfs_lm1 <- get_lfs_lm1(
    path = path,
    all_states = all_states
  )

  lfs_um2 <- get_lfs_um2(
    path = path,
    all_states = all_states
  )

  dplyr::bind_rows(
    lfs_lm1,
    lfs_eq03,
    lfs_um2
  )
}

#' Download and tidy data cube LM1 from detailed labour force
#' @noRd
get_lfs_lm1 <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                        all_states = FALSE) {
  raw_pivot <- get_lfs_pivot("LM1")

  names(raw_pivot) <- c(
    "date",
    "sex",
    "age",
    "marital_status",
    "gcc_restofstate",
    "Employed full-time ('000)",
    "Employed part-time ('000)",
    "Unemployed looked for full-time work ('000)",
    "Unemployed looked for only part-time work ('000)",
    "Not in the labour force (NILF) ('000)"
  )

  tidy_pivot <- raw_pivot %>%
    tidyr::pivot_longer(
      cols = !dplyr::one_of(c(
        "date",
        "sex",
        "age",
        "marital_status",
        "gcc_restofstate"
      )),
      names_to = "indicator",
      values_to = "value"
    )

  if (isFALSE(all_states)) {
    tidy_pivot <- tidy_pivot %>%
      # Drop non-Victoria
      dplyr::filter(.data$gcc_restofstate %in% c(
        "Greater Melbourne",
        "Rest of Vic."
      ))
  }

  # We want to aggregate up various categories - only interested in
  # broad age, employed + unemployed totals (not FT/PT split)
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      age = dplyr::case_when(
        age %in%
          c(
            "15-19 years",
            "20-24 years"
          ) ~ "15-24",
        age %in% c(
          "25-29 years",
          "30-34 years",
          "35-39 years",
          "40-44 years",
          "45-49 years",
          "50-54 years"
        ) ~ "25-54",
        age %in% c(
          "55-59 years",
          "60-64 years",
          "65 years and over"
        ) ~ "55+",
        TRUE ~ NA_character_
      ),
      indicator = dplyr::case_when(
        indicator %in% c(
          "Employed full-time ('000)",
          "Employed part-time ('000)"
        ) ~
        "Employed",
        grepl("Unemployed", .data$indicator) ~
        "Unemployed",
        grepl("NILF", .data$indicator) ~
        "NILF",
        TRUE ~ NA_character_
      )
    )

  tidy_pivot <- tidy_pivot %>%
    dplyr::group_by(
      .data$date, .data$age, .data$gcc_restofstate,
      .data$indicator
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup()


  # Create series IDs
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_id = paste(.data$age,
        .data$gcc_restofstate,
        .data$indicator,
        sep = "_"
      ) %>% tolower(),
      series = paste(.data$indicator,
        .data$gcc_restofstate,
        .data$age,
        sep = " ; "
      ),
      series_type = "Original",
      table_no = "LM1",
      data_type = "STOCK",
      frequency = "Month",
      unit = "000",
      cat_no = "6291.0.55.001"
    )

  tidy_pivot
}

#' Download and tidy data cube EQ03 from detailed Labour Force
#' @noRd
get_lfs_um2 <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                        all_states = FALSE) {
  raw_pivot <- get_lfs_pivot(cube = "UM2")

  names(raw_pivot) <- c(
    "date",
    "duration",
    "state",
    "Unemployed total ('000)",
    "Number of weeks searching for job ('000 Weeks)",
    "Unemployed looked for full-time work ('000)",
    "Unemployed looked for only part-time work ('000)"
  )

  if (isFALSE(all_states)) {
    raw_pivot <- raw_pivot %>%
      dplyr::filter(.data$state == "Victoria")
  }

  tidy_pivot <- raw_pivot %>%
    tidyr::pivot_longer(
      cols = !dplyr::one_of(c(
        "date",
        "state",
        "duration"
      )),
      names_to = "indicator",
      values_to = "value"
    )

  # Create series IDs
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_id = paste(.data$indicator,
        .data$state,
        .data$duration,
        sep = "_"
      ) %>% tolower(),
      series = paste(.data$indicator,
        .data$state,
        .data$duration,
        sep = " ; "
      ),
      series_type = "Original",
      table_no = "UM2",
      data_type = "STOCK",
      frequency = "Month",
      unit = "000",
      cat_no = "6291.0.55.001"
    )

  tidy_pivot
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
  user_timeout <- getOption("timeout")
  on.exit(options("timeout" = user_timeout))
  options("timeout" = 180)

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
    dplyr::mutate(dplyr::across(
      where(~ inherits(.x, "POSIXct")),
      as.Date
    ))

  df
}
