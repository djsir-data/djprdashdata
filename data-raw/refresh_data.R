pkgload::load_all()
library(dplyr)
library(tidyr)

# Load ABS time series
abs_6202 <- read_abs_if_updated(cat_no = "6202.0")
abs_6291 <- read_abs_if_updated(cat_no = "6291.0.55.001")
abs_5368 <- read_abs_if_updated(cat_no = "5368.0")
abs_6345 <- read_abs_if_updated(cat_no = "6345.0")

# Combine LFS data
lfs_tables <- c("6202012"
                # ,
                # "6202016"
                # "6202019",
                # "6202023",
                # "6202025",
                # "6291002",
                # "6291003",
                # "6291024a",
                # "6291025a",
                # "6291005",
                # "6291016",
                # "6291016c",
                # "6291026a"
                )

abs_lfs <- abs_6202 %>%
  filter(table_no %in% lfs_tables)

abs_lfs <- abs_6291 %>%
  filter(table_no %in% lfs_tables) %>%
  bind_rows(abs_lfs)

abs_lfs <- abs_lfs %>%
  group_by(series_id) %>%
  filter(
    # Where a series appears multiple times with different series descriptions,
    # keep the one with the longest description
    nchar(series) == max(nchar(series))
  ) %>%
  # Where a Series ID appears in multiple tables, keep only the first
  filter(table_no == min(table_no)) %>%
  ungroup()

compress_and_save_df(abs_lfs,
                     here::here("data-raw", "abs-ts", "abs-lfs.qs"))

# Save file containing time that this script was last run
last_refreshed <- lubridate::with_tz(Sys.time(), tzone = "Australia/Melbourne")
file_conn <- file(here::here("data-raw", "last_refreshed.txt"))
writeLines(as.character(Sys.time()), file_conn)
close(file_conn)

# Lookup table for LFS series IDs -----
# To re-create it from scratch, set `update_lfs_lookup` to `TRUE`
update_lfs_lookup <- FALSE
if (update_lfs_lookup) {
  source(here::here(
    "data-raw",
    "create_lfs_lookup.R"
  ))
}

lfs_lookup <- readRDS(here::here(
  "data-raw",
  "lfs_lookup.rds"
))

# Save data ----
usethis::use_data(last_refreshed,
  lfs_lookup,
  internal = TRUE,
  overwrite = TRUE
)

usethis::use_data(lfs_lookup,
                  internal = FALSE,
                  overwrite = TRUE)

