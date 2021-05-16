#' Download and tidy ABS pivot cubes
#'
#' @export
#' @param cube Unique fragment of datacube filename, such as "EQ03"
#' @param catalogue_string Such as "labour-force-australia-detailed". See
#' `?readabs::show_available_catalogues` and `?readabs::search_catalogues`.
#' @param path Location to store the raw data cube
#' @param sheet Name of the Excel sheet containing the data for the pivot cube
#' @param col_names Passed to `col_names` argument of `readxl::read_excel()`.
#' If `TRUE`, the default, the text in the spreadsheet will be used as the
#' column names; a character vector can be supplied instead.
#' @examples
#' \dontrun{
#' get_lfs_pivot("EQ03")
#' }


get_lfs_pivot <- function(cube,
                          catalogue_string = "labour-force-australia-detailed",
                          path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                          sheet = "Data 1",
                          col_names = TRUE
                          ) {

  file <- readabs::download_abs_data_cube(catalogue_string = catalogue_string,
                                          cube = cube,
                                          path = path)

  tidy_lfs_pivot(path = file,
                 sheet = sheet,
                 col_names = col_names)

}

tidy_lfs_pivot <- function(path,
                           sheet,
                           col_names) {

  df <- readxl::read_excel(path = path,
                           sheet = sheet,
                           col_names = col_names,
                           skip = 3)

  df
}
