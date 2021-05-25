#' Save compressed data frame
#'
#' Converts any character columns in the data frame to factor, then nests the
#' data frame (so there's one row per `table_no`), then saves
#' using `qs::qsave()`.
#'
#' @param df A data frame that is presumed to contain a `table_no` column and
#' other columns.
#' @param qs_file Path, incl. filename (with "`.qs`" extension) where data
#' frame is saved.
#' @param nest TRUE by default. If TRUE, file will be nested by `series_id`.
#' @details Any character columns (other than `table_no`) will be saved as
#' factors. Loading the data frame with `load_data()` converts factor columns back
#' to character.
#' @returns `TRUE` invisibly on success
#' @export
#' @examples
#' \dontrun{
#'
#' df <- readabs::read_abs("6345.0")
#' save_df(df, "wpi.qs")
#'
#' # Then load the data
#' loaded_df <- load_data("wpi.qs")
#' }
#'
save_df <- function(df, qs_file, nest = TRUE) {

  # Convert strings to factors
  df <- df %>%
    dplyr::mutate_if(
      is.character,
      as.factor
    )

  # Nest, so that each table is a row (makes filtering faster downstream)
  if (isTRUE(nest)) {
    df <- df %>%
      dplyr::nest_by(.data$series_id)
  }

  qs::qsave(x = df, file = qs_file, preset = "high")

  invisible(TRUE)
}
