test_that("read_abs_if_updated() works as expected", {
  skip_if_offline()

  temp_dir <- tempdir()
  on.exit(unlink(temp_dir))

  df <- read_abs_if_updated("6345.0",
    path = temp_dir
  )

  wpi_file <- file.path(temp_dir, "6345-0.qs")

  expect_true(file.exists(wpi_file))
  expect_length(df, 9)
  expect_gt(nrow(df), 50000)

  unnested_df <- qs::qread(wpi_file) %>%
    tidyr::unnest(dplyr::everything()) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(names(df)) %>%
    dplyr::mutate(across(where(is.factor), as.character)) %>%
    dplyr::arrange(series_id, date)

  df <- df %>%
    dplyr::arrange(series_id, date)

  expect_equal(df, unnested_df)

  # Check file is identical when loaded from disk
  df2 <- read_abs_if_updated("6345.0",
    path = temp_dir
  ) %>%
    dplyr::arrange(series_id, date) %>%
    dplyr::relocate(names(df))

  expect_equal(df, df2)
})
