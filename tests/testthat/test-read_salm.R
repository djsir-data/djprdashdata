test_that("read_salm() works", {
  salm <- read_salm()
  expect_length(salm, 9)
  expect_gt(nrow(salm), 20000)
  expect_identical(names(salm),
                   c("date",
                      "value",
                      "series",
                      "series_id",
                      "series_type",
                      "data_type",
                      "table_no",
                      "frequency",
                      "unit")
)
})
