library(readabs)
library(dplyr)
library(purrr)
library(tidyr)
pkgload::load_all()

abs_6202_raw <- read_abs_if_updated(cat_no = "6202.0") %>%
  dplyr::mutate(cat_no = "6202.0")

abs_6291_raw <- read_abs_if_updated(cat_no = "6291.0.55.001") %>%
  dplyr::mutate(cat_no = "6291.0.55.001")

abs_6291_raw <- qs::qread("data-raw/abs-ts/6291-0-55-001.qs")

lfs_pivots <- get_tidy_lfs_pivots()

lfs_lookup <- create_lfs_lookup(
  df_6202 = abs_6202_raw,
  df_6291 = abs_6291_raw,
  lfs_pivots = lfs_pivots
)

saveRDS(lfs_lookup, "data-raw/lfs_lookup.rds")
