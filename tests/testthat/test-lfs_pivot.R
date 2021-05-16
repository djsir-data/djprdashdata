test_that("lfs_pivot functions work", {
  skip_if_offline()

  temppath <- tempdir()
  on.exit(unlink(temppath))

  eq03 <- get_lfs_pivot("EQ03", path = temppath)

  expect_s3_class(eq03, "tbl_df")
  expect_gt(nrow(eq03), 75000)
  expect_length(eq03, 8)

  gm1 <- get_lfs_pivot("GM1", "labour-force-australia", path = temppath)
  expect_gt(nrow(gm1), 1e6)

})
