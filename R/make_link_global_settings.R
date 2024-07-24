
#' Get Global Options for `make_link()`
#'
#' @return List with options in R
#' @export
#'
#' @examples make_link_global_settings_get()
make_link_global_settings_get <- function() {
  getOption("saros")$make_link_defaults
}

#' Set Global Options for `make_link()`
#'
#' @param new List of arguments (see `make_link()`)
#'
#' @return Invisible returned list of old and new values.
#' @export
#'
#' @examples make_link_global_settings_set(new=list(digits=2))
make_link_global_settings_set <- function(new) {
  suppressWarnings(validate_make_link_options(params = new))
  saros_options <- getOption("saros", list())
  current_options <- saros_options$make_link_defaults
  updated_options <- utils::modifyList(current_options, new)
  saros_options$make_link_defaults <- updated_options
  options(saros = saros_options)
  cli::cli_inform("{.val options('saros')$make_link_defaults} has now been set.")
  invisible(list(old = current_options,
                 new = updated_options))
}

#' Reset Global Options for `make_link()`
#'
#' @return Invisible returned list of old and new values.
#' @export
#'
#' @examples make_link_global_settings_reset()
make_link_global_settings_reset <- function() {
  saros_options <- getOption("saros", list())
  old <- saros_options$make_link_defaults
  saros_options$make_link_defaults <- .saros.env$make_link_defaults
  options(saros = saros_options)
  cli::cli_inform("{.val options('saros')$make_link_defaults} has now been reset to factory defaults.")
  invisible(list(old = old,
                 new = saros_options$make_link_defaults))
}

