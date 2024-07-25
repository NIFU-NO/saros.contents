
#' Get Global Options for `makeme()`
#'
#' @return List with options in R
#' @export
#'
#' @examples makeme_global_settings_get()
makeme_global_settings_get <- function() {
  getOption("saros")$makeme_defaults
}

#' Set Global Options for `makeme()`
#'
#' @param new List of arguments (see `makeme()`)
#'
#' @return Invisible returned list of old and new values.
#' @export
#'
#' @examples makeme_global_settings_set(new=list(digits=2))
makeme_global_settings_set <- function(new) {
  suppressWarnings(validate_makeme_options(params = new))
  saros_options <- getOption("saros", list())
  current_options <- saros_options$makeme_defaults
  updated_options <- utils::modifyList(current_options, new)
  saros_options$makeme_defaults <- updated_options
  options(saros = saros_options)
  cli::cli_inform("{.val options('saros')$makeme_defaults} has now been set.")
  invisible(list(old = current_options,
                 new = updated_options))
}

#' Reset Global Options for `makeme()`
#'
#' @return Invisible returned list of old and new values.
#' @export
#'
#' @examples makeme_global_settings_reset()
makeme_global_settings_reset <- function() {
  saros_options <- getOption("saros", list())
  old <- saros_options$makeme_defaults
  saros_options$makeme_defaults <- .saros.env$makeme_defaults
  options(saros = saros_options)
  cli::cli_inform("{.val options('saros')$makeme_defaults} has now been reset to factory defaults.")
  invisible(list(old = old,
                 new = saros_options$makeme_defaults))
}

