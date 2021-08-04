
#' Get all pivot tables for which functions have been created
#' Need to manually add them
#' @param path Path to directory in which to store downloaded file(s)
#' Victoria will be removed.
#'
#'
get_tidy_lfs_pivots <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir())) {
  lfs_eq03 <- get_lfs_eq03(
    path = path
  )

  lfs_lm1 <- get_lfs_lm1(
    path = path
  )

  lfs_um2 <- get_lfs_um2(
    path = path
  )

  lfs_rm1 <- get_lfs_rm1(
    path = path
  )

  dplyr::bind_rows(
    lfs_lm1,
    lfs_eq03,
    lfs_um2,
    lfs_rm1
  )
}

#' Download and tidy data cube LM1 from detailed labour force
#' @noRd
get_lfs_lm1 <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                        all_states = FALSE) {
  raw_pivot <- get_lfs_pivot("LM1",
                             path = path)

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

  age_gcc <- tidy_pivot %>%
    dplyr::group_by(
      .data$date, .data$age, .data$gcc_restofstate,
      .data$indicator
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup()

  age_sex <- tidy_pivot %>%
    dplyr::group_by(
      .data$date, .data$age,
      .data$sex, .data$indicator
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup()


  # Create series IDs
  age_gcc <- age_gcc %>%
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
      ))

  age_sex <- age_sex %>%
    dplyr::mutate(
      series_id = paste(.data$age,
                        .data$sex,
                        .data$indicator,
                        sep = "_"
      ) %>% tolower(),
      series = paste(.data$indicator,
                     .data$sex,
                     .data$age,
                     sep = " ; "
      ))

  tidy_pivot <- dplyr::bind_rows(age_gcc, age_sex) %>%
    dplyr::mutate(dplyr::across(c("sex", "gcc_restofstate"),
                                ~dplyr::if_else(is.na(.x), "", .x)))

  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
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

#' Download and tidy data cube EQ03 from detailed Labour Force
#' @noRd
get_lfs_rm1 <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                        all_states = TRUE) {
  raw_pivot <- get_lfs_pivot(cube = "RM1")

  names(raw_pivot) <- c(
    "date",
    "sex",
    "age",
    "sa4",
    "Employed full-time",
    "Employed part-time",
    "Unemployed",
    "NILF"
  )

  tidy_pivot <- raw_pivot %>%
    tidyr::pivot_longer(
      cols = !dplyr::one_of(c(
        "date",
        "sex",
        "age",
        "sa4"
      )),
      names_to = "indicator",
      values_to = "value"
    )

  # Assign each sa4 to either greater capital city / rest of state
  # Uses lookup table from absmapsdata

  # absmapsdata::sa42011 %>%
  #   select(sa4 = sa4_code_2011, gcc_restofstate = gcc_name_2011) %>%
  #   as_tibble() %>%
  #   filter(!grepl("migratory|usual|other", gcc_restofstate, ignore.case = T))

  sa4_to_gcc <- tibble::tribble(
    ~sa4,               ~gcc_restofstate,
    101L,                  "Rest of NSW",
    102L,               "Greater Sydney",
    103L,                  "Rest of NSW",
    104L,                  "Rest of NSW",
    105L,                  "Rest of NSW",
    106L,                  "Rest of NSW",
    107L,                  "Rest of NSW",
    108L,                  "Rest of NSW",
    109L,                  "Rest of NSW",
    110L,                  "Rest of NSW",
    111L,                  "Rest of NSW",
    112L,                  "Rest of NSW",
    113L,                  "Rest of NSW",
    114L,                  "Rest of NSW",
    115L,               "Greater Sydney",
    116L,               "Greater Sydney",
    117L,               "Greater Sydney",
    118L,               "Greater Sydney",
    119L,               "Greater Sydney",
    120L,               "Greater Sydney",
    121L,               "Greater Sydney",
    122L,               "Greater Sydney",
    123L,               "Greater Sydney",
    124L,               "Greater Sydney",
    125L,               "Greater Sydney",
    126L,               "Greater Sydney",
    127L,               "Greater Sydney",
    128L,               "Greater Sydney",
    201L,                 "Rest of Vic.",
    202L,                 "Rest of Vic.",
    203L,                 "Rest of Vic.",
    204L,                 "Rest of Vic.",
    205L,                 "Rest of Vic.",
    206L,            "Greater Melbourne",
    207L,            "Greater Melbourne",
    208L,            "Greater Melbourne",
    209L,            "Greater Melbourne",
    210L,            "Greater Melbourne",
    211L,            "Greater Melbourne",
    212L,            "Greater Melbourne",
    213L,            "Greater Melbourne",
    214L,            "Greater Melbourne",
    215L,                 "Rest of Vic.",
    216L,                 "Rest of Vic.",
    217L,                 "Rest of Vic.",
    301L,             "Greater Brisbane",
    302L,             "Greater Brisbane",
    303L,             "Greater Brisbane",
    304L,             "Greater Brisbane",
    305L,             "Greater Brisbane",
    306L,                  "Rest of Qld",
    307L,                  "Rest of Qld",
    308L,                  "Rest of Qld",
    309L,                  "Rest of Qld",
    310L,             "Greater Brisbane",
    311L,             "Greater Brisbane",
    312L,                  "Rest of Qld",
    313L,             "Greater Brisbane",
    314L,             "Greater Brisbane",
    315L,                  "Rest of Qld",
    316L,                  "Rest of Qld",
    317L,                  "Rest of Qld",
    318L,                  "Rest of Qld",
    319L,                  "Rest of Qld",
    401L,             "Greater Adelaide",
    402L,             "Greater Adelaide",
    403L,             "Greater Adelaide",
    404L,             "Greater Adelaide",
    405L,                   "Rest of SA",
    406L,                   "Rest of SA",
    407L,                   "Rest of SA",
    501L,                   "Rest of WA",
    502L,                "Greater Perth",
    503L,                "Greater Perth",
    504L,                "Greater Perth",
    505L,                "Greater Perth",
    506L,                "Greater Perth",
    507L,                "Greater Perth",
    508L,                   "Rest of WA",
    509L,                   "Rest of WA",
    601L,               "Greater Hobart",
    602L,                 "Rest of Tas.",
    603L,                 "Rest of Tas.",
    604L,                 "Rest of Tas.",
    701L,               "Greater Darwin",
    702L,                   "Rest of NT",
    801L, "Australian Capital Territory"
  )

  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(sa4 = as.integer(substr(.data$sa4, 1, 3))) %>%
    dplyr::left_join(sa4_to_gcc, by = "sa4")

  # Collapse age groups into three broad groups
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      age =
        dplyr::case_when(
          .data$age == "15-24 years" ~ "15-24",
          .data$age %in% c("25-34 years", "35-44 years", "45-54 years") ~ "25-54",
          .data$age %in% c("55-64 years", "65 years and over") ~ "55+",
          TRUE ~ NA_character_
        )
    )

  # Collapse employment into total
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(indicator = dplyr::if_else(
      .data$indicator %in% c("Employed full-time", "Employed part-time"),
      "Employed",
      .data$indicator
    ))

  # We are not interested in sex differences
  tidy_pivot <- tidy_pivot %>%
    dplyr::group_by(
      .data$date,
      .data$age,
      .data$indicator,
      .data$gcc_restofstate
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup()

  # Create series IDs
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_id = paste(.data$age,
        .data$indicator,
        .data$gcc_restofstate,
        sep = "_"
      ) %>% tolower(),
      series = paste(.data$age,
        .data$indicator,
        .data$gcc_restofstate,
        sep = " ; "
      ),
      series_type = "Original",
      table_no = "RM1",
      data_type = "STOCK",
      frequency = "Month",
      unit = "000",
      cat_no = "6291.0.55.001"
    )

  if (isFALSE(all_states)) {
    tidy_pivot <- tidy_pivot %>%
      dplyr::filter(grepl("Melbourne|Vic", .data$gcc_restofstate))
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
