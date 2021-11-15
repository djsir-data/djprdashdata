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
#' read_salm()
#' @export
#' @importFrom dplyr .data


read_salm <- function(file = tempfile(fileext = ".xlsx")) {

  # Scrape SALM  site
  url <- "https://lmip.gov.au/default.aspx?LMIP/Downloads/SmallAreaLabourMarketsSALM/Estimates"

  lmip_page <- rvest::read_html(url)

  link_text <- lmip_page %>%
    rvest::html_nodes(".download-link") %>%
    rvest::html_text()

  links <- lmip_page %>%
    rvest::html_nodes(".download-link") %>%
    rvest::html_attr("href")

  purrr::map_dfr(c("sa2", "lga"),
                 read_salm_table,
                 links = links,
                 link_text = link_text,
                 file = file)
}

read_salm_table <- function(area_type = "sa2", links, link_text, file) {

  area_type_text <- dplyr::if_else(area_type == "sa2",
                              "SALM SA2 Data",
                              "SALM LGA Data")

  # Find which link on the page contains "small area labour market data

  matching_link <- links[grepl(area_type_text, link_text)]

  matching_link <- paste0("https://lmip.gov.au/", matching_link)

  matching_link <- matching_link[grepl("xlsx", matching_link)]

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

