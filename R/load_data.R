#' Load a data frame saved as a `qs` file and convert factors to strings
#'
#' @param qs_file The path, incl. filename and extension, to a `qs` file.
#'
#' @details Any columns that are factors will be
#' converted to character.
#'
#' Intended for use with dataframes saved using `compress_and_save_df()`.
#' @return A tibble
#' @export
#' @examples
#' \dontrun{
#' load_data("qs_file.qs")
#' }
#'
load_data <- function(qs_file) {
  # Load
  df <- qs::qread(qs_file)

  # Convert factors back to character
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.factor, as.character)

  df
}
