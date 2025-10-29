#' System dependencies
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "proj_golem")
}


#' Access files in the current app
#'
#' NOTE: If you manually change your config file, you'll need to
#' re-run build_config().
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your app.
#'
#' @noRd
app_config <- function(...) {
  config::get(
    value = ...,
    file = app_sys("golem-config.yml")
  )
}
