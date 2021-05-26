test_that("lfs_pivot functions work", {
  skip_if_offline()

  temppath <- tempdir()
  on.exit(unlink(temppath))

  eq03 <- get_lfs_pivot("EQ03", path = temppath)

  expect_s3_class(eq03, "tbl_df")
  expect_gt(nrow(eq03), 75000)
  expect_length(eq03, 8)

  lfs_pivots <- get_tidy_lfs_pivots()

  expect_s3_class(lfs_pivots, "tbl_df")
  expect_gt(nrow(lfs_pivots), 50000)

  # Check that pivots are recent
  time_diff <- Sys.Date() - max(lfs_pivots$date)
  expect_lt(time_diff, 100)
})
