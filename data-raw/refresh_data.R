pkgload::load_all()
library(dplyr)
library(tidyr)
library(rvest)

options(timeout = 180)


# Setup objects for error handling
`%iferror%` <- function(a, b) tryCatch({a}, error = function(e){
  warning(e)
  {b}
  })

attach("R/sysdata.rda")
old_data <- abs_lfs
detach("file:R/sysdata.rda")


# Load LFS data -----
# Define ABS series IDs of interest -----
lfs_ids <- c(
  "A84423349V",
  "A84423356T",
  "A84423355R",
  "A85223451R",
  "A84423354L",
  "A84423350C",
  "A84433601W",
  "A84423357V",
  "A84423244X",
  "A84423468K",
  "A84423043C",
  "A84423272J",
  "A84423286W",
  "A84423370L",
  "A84423328J",
  "A84423300F",
  "A84423314V",
  "A84423342C",
  "A84423242V",
  "A84423466F",
  "A84423270C",
  "A84423368A",
  "A84423340X",
  "A84423326C",
  "A84423284T",
  "A84423312R",
  "A84423298F",
  "A84423050A",
  "A84426256L",
  "A84426277X",
  "A85223450L",
  "A84423051C",
  "A84423271F",
  "A84423369C",
  "A84423341A",
  "A84423327F",
  "A84423285V",
  "A84423313T",
  "A84423299J",
  "A84423689R",
  "A84423461V",
  "A84423237A",
  "A84423245A",
  "A84423469L",
  "A84423238C",
  "A84423462W",
  "A84423577W",
  "A84423801C",
  "A84423351F",
  "A84423239F",
  "A84423463X",
  "A84423243W",
  "A84423467J",
  "A84433602X",
  "A84433603A",
  "A84433505W",
  "A84433503T",
  "A84433504V",
  "A84433519K",
  "A84433517F",
  "A84433518J",
  "A84433533F",
  "A84433531A",
  "A84433532C",
  "A84433617R",
  "A84433615K",
  "A84433616L",
  "A84433575C",
  "A84433573X",
  "A84433574A",
  "A84433547V",
  "A84433545R",
  "A84433546T",
  "A84433589T",
  "A84433587L",
  "A84433588R",
  "A84433561R",
  "A84433559C",
  "A84433560L",
  "A84424692W",
  "A84424622R",
  "A84424691V",
  "A84433475V",
  "A84423687K",
  "A84600253V",
  "A84600145K",
  "A84599659L",
  "A84600019W",
  "A84600187J",
  "A84599557X",
  "A84600115W",
  "A84599851L",
  "A84599923L",
  "A84600025T",
  "A84600193C",
  "A84600079X",
  "A84599665J",
  "A84600031L",
  "A84599671C",
  "A84599677T",
  "A84599683L",
  "A84599929A",
  "A84600121T",
  "A84600037A",
  "A84600141A",
  "A84600075R",
  "A84600144J",
  "A84600078W",
  "A84595516F",
  "A84595471L",
  "A84599655C",
  "A84599658K",
  "A84600015L",
  "A84600018V",
  "A84600183X",
  "A84600186F",
  "A84599553R",
  "A84599556W",
  "A84600111L",
  "A84600114V",
  "A84599847W",
  "A84599850K",
  "A84599919W",
  "A84599922K",
  "A84600021J",
  "A84600024R",
  "A84600189L",
  "A84600192A",
  "A84599661X",
  "A84599664F",
  "A84600027W",
  "A84600030K",
  "A84599667L",
  "A84599670A",
  "A84599673J",
  "A84599676R",
  "A84599679W",
  "A84599682K",
  "A84599925T",
  "A84599928X",
  "A84600117A",
  "A84600120R",
  "A84600033T",
  "A84600036X",
  "A84601680F",
  "A84601683L",
  "A84601686V",
  "A84601665J",
  "A84601704L",
  "A84601707V",
  "A84601710J",
  "A84601638A",
  "A84601653X",
  "A84601689A",
  "A84601656F",
  "A84601713R",
  "A84601668R",
  "A84601695W",
  "A84601698C",
  "A84601650T",
  "A84601671C",
  "A84601641R",
  "A84601716W",
  "A84601662A",
  "A84601681J",
  "A84601684R",
  "A84601687W",
  "A84601666K",
  "A84601705R",
  "A84601708W",
  "A84601711K",
  "A84601639C",
  "A84601654A",
  "A84601690K",
  "A84601657J",
  "A84601714T",
  "A84601669T",
  "A84601696X",
  "A84601699F",
  "A84601651V",
  "A84601672F",
  "A84601642T",
  "A84601717X",
  "A84601663C",
  "A84601682K",
  "A84601685T",
  "A84601688X",
  "A84601667L",
  "A84601706T",
  "A84601709X",
  "A84601712L",
  "A84601640L",
  "A84601655C",
  "A84601691L",
  "A84601658K",
  "A84601715V",
  "A84601670A",
  "A84601697A",
  "A84601700C",
  "A84601652W",
  "A84601673J",
  "A84601643V",
  "A84601718A",
  "A84601664F",
  "A84423091W",
  "A84597687K",
  "A84597693F",
  "A84597723J",
  "A84597729W",
  "A84597681W",
  "A84597699V",
  "A84597705C",
  "A84423089K",
  "A84423691A",
  "A84599625R",
  "A84599781T",
  "A84599607K",
  "A84600243R",
  "A84599715V",
  "A84599631K",
  "A84599628W",
  "A84599629X",
  "A84599630J",
  "A84600080J",
  "A84599784X",
  "A84599785A",
  "A84599786C",
  "A84599718A",
  "A84599719C",
  "A84599720L",
  "A84600246W",
  "A84600247X",
  "A84600248A",
  "A84599634T",
  "A84599635V",
  "A84599636W",
  "A84599610X",
  "A84599611A",
  "A84599612C",
  "A84599660W",
  "A84600020F",
  "A84600188K",
  "A84599558A",
  "A84600116X",
  "A84599852R",
  "A84599924R",
  "A84600026V",
  "A84600194F",
  "A84600146L",
  "A84599623K",
  "A84600151F",
  "A84600157V",
  "A84600241K",
  "A84599791W",
  "A84599656F",
  "A84600016R",
  "A84600184A",
  "A84599554T",
  "A84600112R",
  "A84599848X",
  "A84599920F",
  "A84600022K",
  "A84600190W",
  "A84600142C",
  "A84599666K",
  "A84600032R",
  "A84599672F",
  "A84599678V",
  "A84599684R",
  "A84599930K",
  "A84600122V",
  "A84600038C",
  "A84599647C",
  "A84600259J",
  "A84600091R",
  "A84599815C",
  "A84599662A",
  "A84600028X",
  "A84599668R",
  "A84599674K",
  "A84599680F",
  "A84599926V",
  "A84600118C",
  "A84600034V",
  "A84600076T",
  "A84424687C",
  "A84424777J",
  "A84424785J",
  "A84424786K",
  "A84424778K",
  "A84424780W",
  "A84424598A",
  "A84424600A",
  "A84424688F",
  "A84424689J",
  "A84424694A",
  "A84424597X",
  "A84600252T",
  "A84600254W",
  "A84424695C",
  "A84424696F",
  "A84424781X",
  "A84433594K",
  "A84433597T",
  "A84433476W",
  "A84424602F",
  "A85223482F",
  "A85223418L",
  "A84424601C",

  "A84423054K",
  "A84423265K",
  "A84423279X",
  "A84423363R",
  "A84423321T",
  "A84423293V"
)

stopifnot(length(lfs_ids) > 290)
stopifnot(inherits(lfs_ids, "character"))

# Load files from ABS website using ZIP files
abs_6202 <- dl_and_read("labour-force-australia")
abs_6291 <- dl_and_read("labour-force-australia-detailed")

# Combine LFS data ----

abs_lfs <- abs_6202 %>%
  # We only want series IDs that are defined in the vector above
  filter(series_id %in% lfs_ids)

abs_lfs <- abs_6291 %>%
  # We only want series IDs defined in the vector above AND
  # that are not already taken from ABS 6202
  filter(series_id %in% lfs_ids &
    !series_id %in% abs_lfs$series_id) %>%
  bind_rows(abs_lfs)

abs_lfs <- reduce_ts_df(abs_lfs,
  include_trend = FALSE,
  include_orig_for_sadj = TRUE
)

abs_lfs <- abs_lfs %>%
  group_by(series_id) %>%
  filter(
    # Where a series appears multiple times with different series descriptions,
    # keep the one with the longest description
    nchar(series) == max(nchar(series))
  ) %>%
  mutate(series = as.factor(series)) %>%
  # Where a Series ID appears in multiple tables, keep only the first
  filter(table_no == min(table_no)) %>%
  ungroup()

# Add in series that are 'missing' from the data and must be calculated,
# such as part time employment at state level (calculated from total + full time)
abs_lfs <- add_missing_data(abs_lfs)

# Get pivot tables -------
lfs_pivot <- get_tidy_lfs_pivots()

lfs_pivot <- lfs_pivot %>%
  dplyr::select(
    date, value, series_id, series, series_type,
    table_no, data_type, frequency, unit
  )

abs_lfs <- lfs_pivot %>%
  bind_rows(abs_lfs)

# Get jobactive data -----
jobactive <- read_jobactive() %iferror%
  old_data %>%
  filter(table_no == "jobactive")

stopifnot(length(jobactive) == 9)
stopifnot(nrow(jobactive) > 8000)

abs_lfs <- abs_lfs %>%
  bind_rows(jobactive)

# Get SALM data -----
salm <- read_salm() %iferror%
  old_data %>%
  filter(table_no == "SALM")

stopifnot(length(salm) == 9)
stopifnot(nrow(salm) > 20000)

abs_lfs <- abs_lfs %>%
  bind_rows(salm)


# Get ABS vacancy data

vac <- readabs::read_abs(cat_no = "6354.0") %>%
  filter(
    series_type == "Original",
    table_title != "TABLE 4. Job Vacancies, Industry, Australia ('000) - Original"
    ) %>%
  select(all_of(names(old_data))) %>%
  drop_na()

abs_lfs <- abs_lfs %>%
  bind_rows(vac)


# Get JSA Internet vacancy index
try({
  ivi_tmp_html <- tempfile(fileext = ".html")
  download.file(
    "https://www.jobsandskills.gov.au/work/internet-vacancy-index",
    ivi_tmp_html
  )
  ivi_link <- read_html(ivi_tmp_html) %>%
    html_elements("a.downloadLink") %>%
    html_attr("href") %>%
    stringr::str_subset("xlsx|XLSX") %>%
    stringr::str_subset("regional|Regional|REGIONAL") %>%
    paste0("https://www.jobsandskills.gov.au", .)

  ivi_tmp_xlsx <- tempfile(fileext = ".xlsx")
  download.file(ivi_link, ivi_tmp_xlsx, mode = "wb")

  ivi <- readxl::read_excel(ivi_tmp_xlsx, sheet = "Averaged") %>%
    unite(series, Level, State, region, ANZSCO_CODE) %>%
    select(-ANZSCO_TITLE) %>%
    pivot_longer(
      -series,
      names_to = "date",
      values_to = "value"
    ) %>%
    mutate(
      date = as.Date(as.numeric(date), origin = "1899-12-30"),
      series_id = sapply(series, rlang::hash),
      table_no = "ivi",
      series_type = "Original",
      data_type = "FLOW",
      frequency = "Month",
      unit = "Advertisements"
    )

  abs_lfs <- abs_lfs %>%
    bind_rows(ivi)
})



# Check if data updated -----
new_rows <- nrow(abs_lfs)
# `old_rows` is normally loaded from `sysdata.rda` and will not exist
# if that file was cleared.
data_updated <- !exists("old_rows") || (old_rows != new_rows)

# Perform checks and save ----

try({
  con <- djprConnect::djpr_connect(use_config = TRUE)
})

if (data_updated) {
  test_results <- c(
    all(lfs_ids %in% abs_lfs$series_id),
    max(abs_lfs$date) - max(lfs_pivot$date) < 60,
    Sys.Date() - max(abs_lfs$date) < 100,
    nrow(lfs_pivot) > 51000,
    nrow(abs_lfs) > 107000,
    length(lfs_pivot) == 9,
    length(abs_lfs) == 9
  )

  if (!all(test_results)) {
    stop("Some test results failed. New data not saved.")
  }


  save_df(
    lfs_pivot,
    here::here("data-raw", "abs-ts", "lfs-pivots.qs")
  )

  save_df(
    abs_lfs,
    here::here("data-raw", "abs-ts", "abs-lfs.qs")
  )

  try({

    DBI::dbWriteTable(con, name = 'abs_labour_force',
                      value = mutate(abs_lfs,
                                     timestamp = lubridate::now(tzone = "Australia/Melbourne")),
                      overwrite = TRUE)

  })

}

# Update last_refreshed -----
# Save file containing time that this script was last run.
# Use a fixed timezone so the result is the same whether this is run on
# a cloud machine or a local laptop.
last_refreshed <-
  lubridate::now(tzone = "Australia/Melbourne") |>
  lubridate::format_ISO8601(usetz = T)
writeLines(last_refreshed, here::here("data-raw/last_refreshed.txt"))

if (data_updated) {
  last_updated <- last_refreshed # saved to sysdata.rda
  writeLines(last_updated, here::here("data-raw/last_updated.txt"))
}

# Lookup table for LFS series IDs -----
# To re-create it from scratch, set `update_up` to `TRUE`
update_lfs_lookup <- FALSE
if (update_lfs_lookup) {
  lfs_lookup <- create_lfs_lookup(
    abs_6202,
    abs_6291
  )

  saveRDS(
    lfs_lookup,
    here::here(
      "data-raw",
      "lfs_lookup.rds"
    )
  )
}

lfs_lookup <- readRDS(here::here(
  "data-raw",
  "lfs_lookup.rds"
))

# Save data ----
old_rows <- nrow(abs_lfs)
usethis::use_data(last_refreshed,
  last_updated,
  lfs_lookup,
  abs_lfs,
  old_rows,
  internal = TRUE,
  overwrite = TRUE
)

usethis::use_data(
  lfs_lookup,
  internal = FALSE,
  overwrite = TRUE
)


try({
  DBI::dbWriteTable(
    con,
    name = 'abs_lfs_lookup',
    value = lfs_lookup |>
      dplyr::select(-dplyr::one_of(c(
        "cat_no",
        "table_no",
        "series",
        "series_type"
      ))) |>
      mutate(timestamp = lubridate::now(tzone = "Australia/Melbourne")),
    overwrite = TRUE)

  DBI::dbDisconnect(con)
})
