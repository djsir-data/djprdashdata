test_that("lfs_lookup data object is of expected size", {
  expect_gt(nrow(lfs_lookup), 20000)
  expect_gt(length(lfs_lookup), 27)
})

test_that("create_lfs_lookup() behaves as expected", {
  if (!interactive()) {
    ts_dir <- here::here("djprdashdata", "data-raw", "abs-ts")
  } else {
    ts_dir <- here::here("data-raw", "abs-ts")
  }

  skip_on_ci()

  abs_6202 <- qs::qread(here::here(ts_dir, "6202-0.qs"))
  abs_6291 <- qs::qread(here::here(ts_dir, "6291-0-55-001.qs"))
  abs_pivots <- qs::qread(here::here(ts_dir, "lfs-pivots.qs"))

  new_lookup <- create_lfs_lookup(
    df_6202 = abs_6202,
    df_6291 = abs_6291,
    lfs_pivots = abs_pivots
  )

  expect_gt(nrow(new_lookup), 20000)
  expect_gt(length(new_lookup), 27)
})
