
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

  lfs_em2b <- get_lfs_em2b(
    path = path
  )

  lfs_eq08 <- get_lfs_eq08(
    path = path
  )

  dplyr::bind_rows(
    lfs_lm1,
    lfs_eq03,
    lfs_um2,
    lfs_rm1,
    lfs_em2b,
    lfs_eq08
  )
}

get_lfs_em2b <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                         all_states = FALSE) {
  raw_pivot <- get_lfs_pivot("EM2b",
                             path = path
  )

  names(raw_pivot) <- c("date",
                        "actual_hours_bin",
                        "reason_fewer",
                        "state",
                        "Employed full-time",
                        "Employed part-time",
                        "Number of hours actually worked in all jobs (employed full-time)",
                        "Number of hours actually worked in all jobs (employed part-time)")

  tidy_pivot <- raw_pivot %>%
    tidyr::pivot_longer(
      cols = !dplyr::one_of(c(
        "date",
        "actual_hours_bin",
        "reason_fewer",
        "state"
      )),
      names_to = "indicator",
      values_to = "value"
    )

  if (!all_states) {
    tidy_pivot <- tidy_pivot %>%
      dplyr::filter(.data$state == "Victoria")
  }

  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_type = "Original",
      table_no = "EM2b",
      data_type = "STOCK",
      frequency = "Month",
      unit = "000",
      cat_no = "6291.0.55.001"
    )

  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(series = paste(.data$indicator,
                                 .data$actual_hours_bin,
                                 .data$reason_fewer,
                                 .data$state,
                                 sep = " ; "),
                  series_id = tolower(paste(.data$indicator,
                                            .data$actual_hours_bin,
                                            .data$reason_fewer,
                                            .data$state,
                                    sep = "_"))) %>%
    dplyr::select(-.data$actual_hours_bin,
                  -.data$reason_fewer)

  tidy_pivot
}

#' Download and tidy data cube LM1 from detailed labour force
#' @noRd
get_lfs_lm1 <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                        all_states = FALSE) {
  raw_pivot <- get_lfs_pivot("LM1",
    path = path
  )

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

  # We want to aggregate up various categories - interested in
  # broad age, employed + unemployed totals, looked for FT/PT work and NILF
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
        indicator %in% c("Employed full-time ('000)",
                         "Employed part-time ('000)") ~ "Employed",
                  grepl("NILF", .data$indicator) ~ "NILF",
        indicator == "Unemployed looked for full-time work ('000)" ~ "SearchedFT",
        indicator == "Unemployed looked for only part-time work ('000)" ~ "SearchedPT",
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

  # Calculate unemployment rate
  age_sex <- age_sex %>%
    dplyr::group_by(
      .data$date,
      .data$age,
      .data$sex
    ) %>%
    dplyr::summarise(`Unemployment rate` = 100 * sum(
      (.data$value[.data$indicator == "SearchedFT"] + .data$value[.data$indicator == "SearchedPT"]) /
        (.data$value[.data$indicator == "SearchedFT"] + .data$value[.data$indicator == "SearchedPT"] +
           .data$value[.data$indicator == "Employed"])
      )
    ) %>%
    tidyr::pivot_longer(
      cols = .data$`Unemployment rate`,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unit = "Percent") %>%
    dplyr::bind_rows(age_sex)

  # Calculate Unemployed
  age_sex <- age_sex %>%
    dplyr::group_by(
      .data$date,
      .data$age,
      .data$sex
    ) %>%
    dplyr::summarise(`Unemployed` = sum(
      .data$value[.data$indicator == "SearchedFT"] + .data$value[.data$indicator == "SearchedPT"]
      )
    ) %>%
    tidyr::pivot_longer(
      cols = .data$`Unemployed`,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unit = "'000") %>%
    dplyr::bind_rows(age_sex)

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
      )
    )

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
      )
    )

  tidy_pivot <- dplyr::bind_rows(age_gcc, age_sex) %>%
    dplyr::mutate(dplyr::across(
      c("sex", "gcc_restofstate"),
      ~ dplyr::if_else(is.na(.x), "", .x)
    ))

  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_type = "Original",
      table_no = "LM1",
      data_type = "STOCK",
      frequency = "Month",
      unit = dplyr::if_else(is.na(.data$unit), "000", .data$unit),
      cat_no = "6291.0.55.001"
    )

  tidy_pivot
}

#' Download and tidy data cube um2 from detailed Labour Force
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


#' Download and tidy data cube EQ08 from detailed Labour Force
#' @noRd
get_lfs_eq08 <- function(path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                         all_states = FALSE) {
  raw_pivot <- get_lfs_pivot(cube = "EQ08")

  names(raw_pivot) <- c(
    "date",
    "sex",
    "state",
    "occupation",
    "Employed total",
    "Number of hours actually worked in all jobs"
  )


  tidy_pivot <- raw_pivot %>%
    tidyr::pivot_longer(
      cols = !dplyr::one_of(c(
        "date",
        "sex",
        "state",
        "occupation"
      )),
      names_to = "indicator",
      values_to = "value"
    )

  #Collapse Ocuupation into 8 major groups
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(occupation = stringr::str_sub(occupation, 1, 1)) %>%
    dplyr::mutate(occupation = dplyr::case_when(
      occupation == '1' ~'Managers',
      occupation == '2' ~ 'Professionals',
      occupation == '3' ~ 'Technicians and Trades Workers',
      occupation == '4' ~ 'Community and Personal Service Workers',
      occupation == '5' ~ 'Clerical and Administrative Workers',
      occupation == '6' ~ 'Sales Workers',
      occupation == '7' ~ 'Machinery Operators and Drivers',
      occupation == '8' ~ 'Labourers'
    )) %>%
  dplyr::group_by(.data$date,.data$occupation,.data$sex,
          .data$indicator, .data$state
  ) %>%
    dplyr::summarise(value= sum(value)) %>%
    dplyr::ungroup()


  if (isFALSE(all_states)) {
    tidy_pivot <-  tidy_pivot%>%
      dplyr::filter(.data$state == "Victoria")

  }


  # Create series IDs
  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_id = paste(.data$sex,
        .data$state,
        .data$occupation,
        .data$indicator,
        sep = "_"
      ) %>% tolower(),
      series = paste(.data$indicator,
        .data$state,
        .data$sex,
        .data$occupation,
        sep = " ; "
      ),
      series_type = "Original",
      table_no = "EQ08",
      data_type = "STOCK",
      frequency = "Quarter",
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
      frequency = "Quarter",
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

#' Download and tidy data cube rm1 from detailed Labour Force
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

  sa4_lookup <- raw_pivot %>%
    dplyr::group_by(.data$sa4) %>%
    dplyr::summarise() %>%
    tidyr::separate(
      col = .data$sa4,
      into = c("sa4_code", "sa4_name"),
      sep = " ",
      extra = "merge"
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
  gcc <- tidy_pivot %>%
    dplyr::group_by(
      .data$date,
      .data$age,
      .data$indicator,
      .data$gcc_restofstate
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup()

  # Create series IDs
  gcc <- gcc %>%
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
      )
    )

  sa4 <- tidy_pivot %>%
    dplyr::group_by(
      .data$date,
      .data$age,
      .data$indicator,
      .data$sa4
    ) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sa4 = as.character(.data$sa4)) %>%
    dplyr::left_join(sa4_lookup,
      by = c("sa4" = "sa4_code")
    ) %>%
    # dplyr::select(date, age, indicator, sa4 = sa4_name, value) %>%
    dplyr::filter(.data$age == "15-24")

  sa4 <- sa4 %>%
    dplyr::mutate(
      series_id = paste(
        .data$age,
        .data$indicator,
        .data$sa4_name,
        sep = "_"
      ) %>% tolower(),
      series = paste(.data$age,
        .data$indicator,
        .data$sa4_name,
        sep = " ; "
      )
    )

  tidy_pivot <- dplyr::bind_rows(sa4, gcc)

  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(dplyr::across(
      c(.data$sa4, .data$gcc_restofstate),
      ~ dplyr::if_else(is.na(.x), "", .x)
    ))

  tidy_pivot <- tidy_pivot %>%
    dplyr::mutate(
      series_type = "Original",
      table_no = "RM1",
      data_type = "STOCK",
      frequency = "Month",
      unit = "000",
      cat_no = "6291.0.55.001"
    )

  if (isFALSE(all_states)) {
    tidy_pivot <- tidy_pivot %>%
      dplyr::filter(grepl("Melbourne|Vic", .data$gcc_restofstate) |
        .data$gcc_restofstate == "")
  }

  tidy_pivot <- tidy_pivot %>%
    dplyr::filter(substr(.data$sa4, 1, 1) == "2" |
      .data$sa4 == "")

  tidy_pivot <- tidy_pivot %>%
    dplyr::select(-.data$sa4) %>%
    dplyr::rename(sa4 = .data$sa4_name)

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
