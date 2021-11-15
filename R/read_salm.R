#' Download and tidy SALM data from the LMIP
#'
#' The Commonwealth Government publishes small area labour market aata publicly on the
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


read_salm <- function(file = tempfile(fileext = ".xlsx")) {

  # Scrape SALM  site
  url <- "https://lmip.gov.au/default.aspx?LMIP/Downloads/SmallAreaLabourMarketsSALM/Estimates"

lmip_page <-  rvest::read_html(url)

link_text <- lmip_page %>%
  rvest::html_nodes(".download-link") %>%
  rvest::html_text()

links <- lmip_page %>%
          rvest::html_nodes(".download-link") %>%
          rvest::html_attr("href")

# Find which link on the page contains "small area labour market data

matching_link <- links[grepl("SALM SA2 Data", link_text)]

matching_link <- paste0("https://lmip.gov.au/", matching_link)

excel_location <-tempfile(fileext = ".xlsx")

# Download the small labour market area data Excel file

download.file(url= matching_link[grepl("xlsx", matching_link)],
              destfile = excel_location,
              mode = "wb")

# Load the Excel file
raw_data <- readxl::read_excel(excel_location,
                               skip = 3)

# change to character and prepare to pivot longer
raw_data[,3:ncol(raw_data)] <- (sapply(raw_data[,3:ncol(raw_data)], as.character))


# change the excel date to r readeable
df_SA2x <- raw_data %>%
          tidyr::pivot_longer(3:ncol(raw_data),
               names_to = "date",
               values_to = "value") %>%
          dplyr::mutate(date = janitor::excel_numeric_to_date(as.numeric(date)),
              value = as.numeric(value))

Victoria_sa2 <- df_SA2x %>%
  dplyr::filter(value != "NA") %>%
  dplyr::filter(substr(`SA2 Code (2016 ASGS)`,1,1) == "2")

Victoria_sa2
}
