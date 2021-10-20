
read_jobactive <- function() {

  url <- "https://lmip.gov.au/default.aspx?LMIP/Downloads/EmploymentRegion"

  lmip_page <- read_html(url)

  link_text <- lmip_page %>%
    html_nodes(".download-link") %>%
    html_text()

  #to get the link
  links <- lmip_page %>%
    html_nodes(".download-link") %>%
    html_attr("href")

  matching_link <- links[grepl("jobactive Caseload Data", link_text)]

  matching_link <- paste0("https://lmip.gov.au/", matching_link)

  excel_location <- tempfile(fileext =".xlsx")

  download.file(url = matching_link,
                destfile = excel_location,
                mode = "wb")

  raw_data <- readxl::read_excel(excel_location,
                                 col_names = FALSE,
                                 skip = 1)

  raw_data <- raw_data %>%
    filter(row_number() != max(row_number()))

  dates <- raw_data %>%
    filter(row_number() == 1) %>%
    pivot_longer(names_to = "col",
                 values_to = "date",
                 cols = everything())

  df <- raw_data %>%
    filter(row_number() != 1) %>%
    pivot_longer(names_to = "col",
                 values_to = "value",
                 cols = !1)

  df <- df %>%
    left_join(dates, by = "col") %>%
    mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
    fill(date)

  df <- df %>%
    pivot_wider(names_from = 1,
                values_from = value) %>%
    select(-col) %>%
    rename(indicator = `Employment Region Name`)

  df <- df %>%
    pivot_longer(names_to = "region",
                 values_to = "value",
                 cols = !one_of(c("date", "indicator")))

  df <- df %>%
    mutate(value = as.numeric(value))


}
