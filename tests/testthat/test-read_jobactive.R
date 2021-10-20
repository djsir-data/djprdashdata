test_that("read_jobactive returns expected output", {
  ja <- read_jobactive()
  expect_s3_class(ja, "tbl_df")
  expect_length(ja, 4)
  expect_identical(names(ja),
                   c("date", "indicator", "region", "value"))
  expect_gte(nrow(ja), 8305)
})
