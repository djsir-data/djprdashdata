pkgload::load_all()

# Load ABS time series
abs_6202 <- read_abs_if_updated(cat_no = "6202.0")
abs_6345 <- read_abs_if_updated(cat_no = "6345.0")

# Save file containing time that this script was last run
last_refreshed <- format(Sys.time(), tz = "Australia/Melbourne")
file_conn <- file(here::here("data-raw", "last_refreshed.txt"))
writeLines(as.character(Sys.time()), file_conn)
close(file_conn)

# usethis::use_data(last_refreshed, overwrite = TRUE)
