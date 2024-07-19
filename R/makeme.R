#' Method for Creating Saros Contents
#'
#' Takes the same arguments as `sarosmake`, except
#' that dep and indep in makeme are character vectors,
#' for ease of user-customized function programming.
#'
#' @param type *Method name*
#'
#'   `scalar<character>` with a class named by itself.
#'
#'   Optional string indicating the specific method. Occasionally
#'   useful for error messages, etc.
#'
#' @param ... *Dots*
#'
#'   Arguments provided by `sarosmake`
#'
#'
#' @export
makeme <- function(type, ...) {
  class(type) <- type
  UseMethod("makeme", type)
}


#' @export
makeme.default <- function(type, ...) {
  dots <- rlang::list2(...)
  cli::cli_abort("Invalid makeme-type: {type}. Check that you have loaded the required packages/methods and registererd your custom method.")
}
