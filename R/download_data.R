#' Download file from the GitHub repo for this package
#'
#' @param path path + filename for a qs file incl. extension,
#' with forward slashes, such as "data-raw/abs-ts/6202-0.qs". Path should
#' be relative to the root directory of this package/repo.
#' @examples
#' \dontrun{
#' download_data("data-raw/abs-ts/6202-0.qs")
#' }
#' @return A dataframe
#' @export

download_data <- function(path) {
  github_prefix <- "https://github.com/djpr-data/djprdashdata/blob/main/"

  qs_url <- paste0(github_prefix, path, "?raw=true")

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
#' @return A dataframe
#' @details `download_abs_ts()` presumes that a file corresponding to the
#' requested catalogue number is present in the GitHub repo for this package.
#' @examples
#' \dontrun{
#' download_abs_ts("6202.0")
#' }
#' @export
download_abs_ts <- function(cat_no) {
  cat_no <- gsub("\\.", "\\-", cat_no)

  path <- paste0("data-raw/abs-ts/", cat_no, ".qs")

  download_data(path = path)
}
