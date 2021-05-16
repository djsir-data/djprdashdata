pkgload::load_all()
library(dplyr)
library(tidyr)

# Load ABS time series
abs_6202 <- read_abs_if_updated(cat_no = "6202.0")
abs_6291 <- read_abs_if_updated(cat_no = "6291.0.55.001")
abs_5368 <- read_abs_if_updated(cat_no = "5368.0")
abs_6345 <- read_abs_if_updated(cat_no = "6345.0")

# Combine LFS data
lfs_ids <- c("A84423349V",
             "A84423043C",
             "A84423357V",
             "A84423356T",
             "A84423244X",
             "A84423468K",
             "A84423354L",
             "A84423050A",
             "A84423270C",
             "A84423368A",
             "A84423340X",
             "A84423326C",
             "A84423284T",
             "A84423312R",
             "A84423298F",
             "A84423355R",
             "A84423051C",
             "A84423271F",
             "A84423369C",
             "A84423341A",
             "A84423327F",
             "A84423285V",
             "A84423313T",
             "A84423187R",
             "A84426256L",
             "A84426277X",
             "A85223450L",
             "A85223451R",
             "A84423237A",
             "A84423461V",
             "A84423245A",
             "A84423469L",
             "A84423350C",
             "A84423238C",
             "A84423462W",
             "A84423243W",
             "A84423467J",
             "A84423242V",
             "A84423466F",
             "A84424692W",
             "A84424691V",
             "A84424621L",
             "A84424600A",
             "A84424601C",
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
             "A84600027W",
             "A84599661X",
             "A84599667L",
             "A84599673J",
             "A84599679W",
             "A84600189L",
             "A84599925T",
             "A84600117A",
             "A84600033T",
             "A84599655C",
             "A84600015L",
             "A84600183X",
             "A84599553R",
             "A84600111L",
             "A84599847W",
             "A84599919W",
             "A84600021J",
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
             "A84423351F",
             "A84423239F",
             "A84423463X",
             "A84423689R",
             "A84423577W",
             "A84423801C")

abs_lfs <- abs_6202 %>%
  filter(series_id %in% lfs_ids)

abs_lfs <- abs_6291 %>%
  filter(series_id %in% lfs_ids &
           !series_id %in% abs_lfs$series_id) %>%
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

# Add in series that are 'missing' from the data and must be calculated,
# such as part time employment at state level (calculated from total + full time)
abs_lfs <- add_missing_data(abs_lfs)


compress_and_save_df(abs_lfs,
                     here::here("data-raw", "abs-ts", "abs-lfs.qs"))

# Get pivot tables
eq03 <- get_lfs_eq03()
lfs_pivot <- bind_rows(eq03)
compress_and_save_df(lfs_pivot,
                     here::here("data-raw", "abs-ts", "abs-lfs-pivot.qs"))


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

