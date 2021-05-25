#' Download file from the GitHub repo for this package
#'
#' @param path path + filename for a qs file incl. extension,
#' with forward slashes, such as "data-raw/abs-ts/6202-0.qs". Path should
#' be relative to the root directory of this package/repo.
#' @param branch Github branch of this repo from which to download a file;
#' default is `main`.
#' @examples
#' \dontrun{
#' download_data("data-raw/abs-ts/6202-0.qs")
#' }
#' @return A dataframe
#' @export

download_data <- function(path, branch = "main") {
  github_prefix <- "https://github.com/djpr-data/djprdashdata/blob/"

  qs_url <- paste0(
    github_prefix,
    branch,
    "/",
    path,
    "?raw=true"
  )

  temp_qs <- tempfile(fileext = ".qs")

  utils::download.file(
    url = qs_url,
    destfile = temp_qs,
    quiet = TRUE,
    mode = "wb",
    cacheOK = FALSE
  )

  df <- load_data(temp_qs)

  df
}

#' Download and import a dataframe
#' @param cat_no ABS catalogue number such as "6202.0"
#' @param branch Name of the Github branch of this repo from which to download file
#' @return A dataframe
#' @details `download_abs_ts()` presumes that a file corresponding to the
#' requested catalogue number is present in the GitHub repo for this package.
#' @examples
#' \dontrun{
#' download_abs_ts("6202.0")
#' }
#' @export
download_abs_ts <- function(cat_no, branch = "main") {
  cat_no <- gsub("\\.", "\\-", cat_no)

  path <- paste0("data-raw/abs-ts/", cat_no, ".qs")

  download_data(path = path, branch = branch)
}

#' Load a data frame saved as a `qs` file and convert factors to strings
#'
#' @param qs_file The path, incl. filename and extension, to a `qs` file.
#'
#' @details Any columns that are factors will be
#' converted to character.
#'
#' Intended for use with dataframes saved using `save_df()`.
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
