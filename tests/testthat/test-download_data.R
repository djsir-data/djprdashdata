test_that("download_data() returns nested tibble", {
  dash_data <- download_abs_ts("abs-lfs")
  expect_s3_class(dash_data, "tbl_df")
  expect_s3_class(dash_data$data, "vctrs_vctr")
  expect_gt(nrow(dash_data), 526)
  expect_equal(length(dash_data), 2)

  unnested_dash <- tidyr::unnest(dash_data, cols = data)

  expect_lt(
    Sys.Date() - max(unnested_dash$date),
    100
  )
  expect_gt(nrow(unnested_dash), 100000)
  expect_equal(length(unnested_dash), 9)
})
