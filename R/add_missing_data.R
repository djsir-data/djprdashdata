#' Function to add missing data to the LFS
#'
#' For example, employment is reported at state level for totals and
#' for full-time, but not for part-time. We calculate part-time.
#'
#' @param data Data frame containing ABS LFS data
#'
#' @return A data frame with additional rows corresponding to the added data

add_missing_data <- function(data) {
  out <- add_part_time(data)

  out
}

#' Function to add missing part-time employment numbers at state level
#' Calculated based on totals and full time numbers
#' @param data data frame
#' @noRd

add_part_time <- function(data) {
  raw_df <- data

  out <- data %>%
    dplyr::filter(.data$series_id %in% c("A84423349V", "A84423357V")) %>%
    dplyr::select(dplyr::all_of(c("series", "date", "value"))) %>%
    tidyr::pivot_wider(names_from = .data$series, values_from = .data$value) %>%
    dplyr::mutate(value = .data$`Employed total ;  Persons ;  > Victoria ;` -
      .data$`> Employed full-time ;  Persons ;  > Victoria ;`) %>%
    dplyr::select(.data$date, .data$value) %>%
    dplyr::mutate(
      series = "Employed part-time ;  Persons ;  Victoria",
      series_id = "pt_emp_vic"
    )

  out <- raw_df %>%
    dplyr::filter(.data$series_id == "A84423349V") %>%
    dplyr::select(-.data$value, -.data$series, -.data$series_id) %>%
    dplyr::right_join(out, by = "date")

  dplyr::bind_rows(raw_df, out)
}
