test_that("dl_and_unzip downloads and unzips", {
  temp_excel_dir <- tempdir()
  on.exit(unlink(temp_excel_dir))
  ts_dir <- file.path(temp_excel_dir, "time-series")

  dl_and_unzip("labour-force-australia",
    dest_dir = temp_excel_dir
  )

  ts_files <- list.files(ts_dir, full.names = TRUE)

  # Test Excel files exist
  expect_true(dir.exists(ts_dir))
  expect_true(all(tools::file_ext(ts_files) %in% c("xls", "xlsx")))

  # Test Excel files are non-zero size
  expect_gt(min(file.size(ts_files)), 50000)

  # Test time series spreadsheets are in expected form
  tidy_ts <- readabs::read_abs_local(
    filenames = list.files(ts_dir),
    path = ts_dir
  )

  expect_s3_class(tidy_ts, "tbl")
  expect_gt(nrow(tidy_ts), 2000000)
  expect_length(tidy_ts, 12)
})
