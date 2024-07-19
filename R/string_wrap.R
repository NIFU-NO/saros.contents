string_wrap <- function(str, width) {
  unlist(
    lapply(
      stringi::stri_wrap(str = str, width = width, simplify = F),
      FUN = function(.x) paste0(.x, collapse="\n")))
}
