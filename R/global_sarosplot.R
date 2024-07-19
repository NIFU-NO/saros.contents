
#' Get Global Options for `sarosmake()`
#'
#' @return List with options in R
#' @export
#'
#' @examples sarosmake_global_settings_get()
sarosmake_global_settings_get <- function() {
  getOption("saros")$sarosmake_defaults
}

#' Set Global Options for `sarosmake()`
#'
#' @param new List of arguments (see `sarosmake()`)
#'
#' @return Invisible returned list of old and new values.
#' @export
#'
#' @examples sarosmake_global_settings_set(new=list(digits=2))
sarosmake_global_settings_set <- function(new) {
  suppressWarnings(validate_sarosmake_options(params = new))
  saros_options <- getOption("saros", list())
  current_options <- saros_options$sarosmake_defaults
  updated_options <- utils::modifyList(current_options, new)
  saros_options$sarosmake_defaults <- updated_options
  options(saros = saros_options)
  cli::cli_inform("{.val options('saros')$sarosmake_defaults} has now been set.")
  invisible(list(old = current_options,
                 new = updated_options))
}

#' Reset Global Options for `sarosmake()`
#'
#' @return Invisible returned list of old and new values.
#' @export
#'
#' @examples sarosmake_global_settings_reset()
sarosmake_global_settings_reset <- function() {
  saros_options <- getOption("saros", list())
  old <- saros_options$sarosmake_defaults
  saros_options$sarosmake_defaults <- .saros.env$sarosmake_defaults
  options(saros = saros_options)
  cli::cli_inform("{.val options('saros')$sarosmake_defaults} has now been reset to factory defaults.")
  invisible(list(old = old,
                 new = saros_options$sarosmake_defaults))
}

