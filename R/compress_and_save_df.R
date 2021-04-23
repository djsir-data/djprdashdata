#' Compress and save a data frame
#'
#' Converts any character columns in the data frame to factor, then nests the
#' data frame (so there's one row per `table_no`), then saves
#' using `qs::qsave()`.
#'
#' @param df A data frame that is presumed to contain a `table_no` column and
#' other columns.
#' @param qs_file Path, incl. filename (with "`.qs`" extension) where data
#' frame is saved.
#' @details Any character columns (other than `table_no`) will be saved as
#' factors. Loading the data frame with `load_data()` converts factor columns back
#' to character.
#' @returns `TRUE` invisibly on success
#' @export
#' @examples
#' \dontrun{
#'
#' df <- readabs::read_abs("6345.0")
#' compress_and_save_df(df, "wpi.qs")
#'
#' # Then load the data
#' loaded_df <- load_data("wpi.qs")
#' }

compress_and_save_df <- function(df, qs_file) {

  # Convert strings to factors
  df <- df %>%
    dplyr::mutate_if(is.character,
                     as.factor)

  # Nest, so that each table is a row
  df <- df %>%
    dplyr::nest_by(.data$table_no)

  qs::qsave(x = df, file = qs_file)

  invisible(TRUE)
}
