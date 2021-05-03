test_that("find_lfs_series() returns expected output", {
  expect_length(find_lfs_series("foobar"),
                0)
  expect_length(find_lfs_series("unemployment rate"),
                1)
  expect_identical(find_lfs_series("unemployment rate"),
                   "A84423050A")
  expect_identical(find_lfs_series("unemployment rate",
                                   sex = "Females"),
                   "A84423064R")
  expect_identical(find_lfs_series("unemployment rate",
                                   sex = "females",
                                   state = "victoria"),
                   "A84423466F")

})
