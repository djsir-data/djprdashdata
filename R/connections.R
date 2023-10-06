


#' @title DJPR PostgreSQL Database Connections
#' @description Connect to the DJPR AWS PostgreSQL server and databases. All
#' users must have authentication details stored as environment variables.
#' You can add these via [usethis::edit_r_environ()]
#'
#' @param db
#' @param user
#' @param use_config
#' @param ... additional arguments passed to [config::get()]
#'
#' @import DBI
#' @import glue
#' @import config
#' @importFrom assertthat assert_that
#' @importFrom RPostgres Postgres
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' djpr_connect()
#' }
djpr_connect <- function(db = c("opendata", "official_sensitive"), user = c('open','super'), use_config = FALSE, ...){

  # override config option based on context
  if (Sys.getenv('R_CONFIG_ACTIVE') == 'github') use_config <- TRUE

  if (use_config) {

    # look for existing credentials
    creds <- tryCatch({config::get("dataconnection", ...)},
                      error = function(e){
                        message(e)
                        NULL
                      })
    if (!is.null(creds)) {
      for (i in 1:length(creds)) assign(toupper(names(creds)[i]), creds[[i]])
    } else {
      assertthat::assert_that(FALSE, msg = 'config.yml file could not be found')
    }

  } else {

    db <- match.arg(db)
    user <- match.arg(user)

    if (db == "official_sensitive") assertthat::assert_that(user == 'super', msg = glue('You must use super user credientials to access the {db} database'))

    if (user == 'open') {
      USER_ENV <- 'PG_READ_OPEN_USER'
      PW_ENV <- 'PG_READ_OPEN_PW'
    } else if (user == 'super') {
      USER_ENV <- 'PG_SUSER'
      PW_ENV <- 'PG_SUSER_PW'
    }

    # check environment variables exists
    assertthat::assert_that(USER_ENV %in% names(Sys.getenv()),
                            msg = glue::glue('User not found, use usethis::edit_r_environ() to add a {USER_ENV} environment variable'))
    assertthat::assert_that(PW_ENV %in% names(Sys.getenv()),
                            msg = glue::glue('Password not found, use usethis::edit_r_environ() to add a {PW_ENV} environment variable'))

    HOST <- '10.210.1.26'
    DBNAME <- db
    USER <- Sys.getenv(USER_ENV)
    PASSWORD <- Sys.getenv(PW_ENV)
    PORT <- 443
    USE_DBCACHE <- FALSE # not currently used

  }

  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = HOST,
    dbname = DBNAME,
    user = USER,
    password = PASSWORD,
    port = PORT
  )

}
