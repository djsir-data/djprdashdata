#' Download and tidy SALM data from the LMIP
#'
#' The Commonwealth Government publishes small area labour market data publicly on the
#' Labour Market Information Portal. This function downloads and tidies that
#' data.
#'
#' @param file Path, including filename and extension, to the location where
#' the small area labour markt Excel file should be downloaded.
#' @return A `tbl_df`
#' @examples
#' \dontrun{
#' read_salm()
#' }
#' @export
#' @importFrom dplyr .data
#' @import djprdata


read_salm <- function(file = tempfile(fileext = ".xlsx")) {

  # Scrape SALM  site
  url <- "https://www.jobsandskills.gov.au/work/small-area-labour-markets"

  # lmip_page <- rvest::read_html(url)
  #
  # link_text <- lmip_page %>%
  #   rvest::html_nodes(".download-link") %>%
  #   rvest::html_text()

  links <- djprdata:::get_latest_download_url(url,
                                              'SALM%20Smoothed%20SA2|SALM%20Smoothed%20LGA')$url

  salm <- purrr::map_dfr(c("sa2", "lga"),
                 read_salm_table,
                 links = links,
                 file = file)

  salm <- salm %>%
    dplyr::mutate(series = paste(
      "SALM",
      'area_type',
      'area_code',
      'area',
      sep = " ; "
    ),
    series_id = tolower(paste(
      "salm",
      'area_type',
      'area_code',
      'area',
      sep = "_"
    )),
    series_type = "Seasonally Adjusted",
    data_type = "STOCK",
    table_no = "SALM",
    frequency = "Quarter",
    unit = "Percent"
    ) %>%
    dplyr::select(
      'date', 'value', 'series',
      'series_id', 'series_type', 'data_type',
      'table_no', 'frequency', 'unit'
    )

  salm

}

read_salm_table <- function(area_type = "sa2", links, file) {

  # area_type_text <- dplyr::if_else(area_type == "sa2",
  #                             "SALM SA2 Data",
  #                             "SALM LGA Data")

  # Find which link on the page contains "small area labour market data
  matching_link <- links[grepl(area_type, links, ignore.case = TRUE) & grepl('\\.xlsx', links)]

  # Download the small labour market area data Excel file

  utils::download.file(
    url = matching_link,
    destfile = file,
    mode = "wb"
  )

  # Load the Excel file
  raw_data <- readxl::read_excel(file,
                                 skip = 3
  )

  # change to character and prepare to pivot longer
  raw_data <- raw_data %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                as.character))

  # change the Excel date to dates
  df <- raw_data %>%
    tidyr::pivot_longer(3:ncol(raw_data),
                        names_to = "date",
                        values_to = "value"
    ) %>%
    dplyr::mutate(
      date = janitor::excel_numeric_to_date(as.numeric(.data$date)),
      value = suppressWarnings(as.numeric(.data$value))
    )

  df <- df %>%
    dplyr::rename(area = 1, area_code = 2) %>%
    dplyr::mutate(area_type = area_type)

  vic <- df %>%
    dplyr::filter(.data$value != "NA") %>%
    dplyr::filter(substr(.data$area_code, 1, 1) == "2")

  vic
}

