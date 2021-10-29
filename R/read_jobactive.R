#' Download and tidy jobactive caseload data from the LMIP
#'
#' The Commonwealth Government publishes jobactive caseload data publicly on the
#' Labour Market Information Portal. This function downloads and tidies that
#' data.
#'
#' @param file Path, including filename and extension, to the location where
#' the jobactive Excel file should be downloaded.
#' @return A `tbl_df`
#' @examples
#' \dontrun{
#' read_jobactive()
#' }
#' @export
#' @importFrom dplyr .data

read_jobactive <- function(file = tempfile(fileext = ".xlsx")) {

  # Scrape Jobactive site
  url <- "https://lmip.gov.au/default.aspx?LMIP/Downloads/EmploymentRegion"

  lmip_page <- rvest::read_html(url)

  lmip_nodes <- lmip_page %>%
    rvest::html_nodes(".download-link")

  link_text <- lmip_nodes %>%
    rvest::html_text()

  links <- lmip_nodes %>%
    rvest::html_attr("href")

  # Find which link on the page contains "jobactive caseload data"
  matching_link <- links[grepl("jobactive Caseload Data",
    link_text,
    ignore.case = TRUE
  )]

  matching_link <- paste0("https://lmip.gov.au/", matching_link)

  # Download the jobactive caseload data Excel file

  utils::download.file(
    url = matching_link,
    destfile = file,
    quiet = TRUE,
    mode = "wb"
  )

  # Load the Excel file
  raw_data <- suppressMessages(
    readxl::read_excel(file,
      col_names = FALSE,
      skip = 1,
      .name_repair = "unique"
    )
  )

  # Remove the final row, as it contains notes
  raw_data <- raw_data %>%
    dplyr::filter(!grepl("NOTES", .data$...1))

  # The first row contains dates - extract this and pivot to long format
  dates <- raw_data %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    tidyr::pivot_longer(
      names_to = "col",
      values_to = "date",
      cols = dplyr::everything()
    )

  df <- raw_data %>%
    dplyr::filter(dplyr::row_number() != 1) %>%
    tidyr::pivot_longer(
      names_to = "col",
      values_to = "value",
      cols = !1
    )

  df <- df %>%
    dplyr::left_join(dates, by = "col") %>%
    dplyr::mutate(date = janitor::excel_numeric_to_date(as.numeric(.data$date))) %>%
    tidyr::fill(.data$date)

  df <- df %>%
    tidyr::pivot_wider(
      names_from = 1,
      values_from = .data$value
    ) %>%
    dplyr::select(-.data$col) %>%
    dplyr::rename(indicator = .data$`Employment Region Name`)

  df <- df %>%
    tidyr::pivot_longer(
      names_to = "region",
      values_to = "value",
      cols = !dplyr::one_of(c("date", "indicator"))
    )

  df <- df %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value)) / 1000)

  df <- df %>%
    dplyr::mutate(
      indicator = gsub("Caseload", "", .data$indicator, ignore.case = TRUE),
      indicator = gsub("jobactive", "", .data$indicator, ignore.case = TRUE)
    )

  df <- df %>%
    dplyr::mutate(
      indicator = gsub("(15+)", "", .data$indicator, fixed = TRUE),
      indicator = gsub("under 25", "15-24", .data$indicator, fixed = TRUE),
      indicator = stringr::str_squish(.data$indicator)
    )

  df <- df %>%
    dplyr::mutate(
      series = paste("Jobactive caseload", .data$indicator, .data$region, sep = " ; "),
      series_id = tolower(paste("jobactive", .data$indicator, .data$region, sep = "_")),
      series_type = "Original",
      data_type = "STOCK",
      table_no = "jobactive",
      frequency = "Quarter",
      unit = "000"
    ) %>%
    dplyr::select(-.data$indicator, -.data$region)

  df
}

#'

create_jobactive_lookup <- function(jobactive) {

}
