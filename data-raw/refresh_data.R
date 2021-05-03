pkgload::load_all()

# Load ABS time series
abs_6202 <- read_abs_if_updated(cat_no = "6202.0")
abs_6291 <- read_abs_if_updated(cat_no = "6291.0.55.001")
abs_5368 <- read_abs_if_updated(cat_no = "5368.0")
# abs_6345 <- read_abs_if_updated(cat_no = "6345.0")

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
