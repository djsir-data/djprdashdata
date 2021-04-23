pkgload::load_all()

abs_6202 <- read_abs_if_updated(cat_no = "6202.0")

last_refreshed <- Sys.time()

file_conn <- file(here::here("data-raw", "last_refreshed.txt"))
writeLines(as.character(Sys.time()), file_conn)
close(file_conn)

# usethis::use_data(last_refreshed, overwrite = TRUE)
