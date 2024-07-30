#' Obtain range of N for a given data set and other settings.
#'
#'
#' @param data Dataset
#' @param dep,indep Character vector, names of (in)dependent variables
#' @param glue_template_1,glue_template_2 String, for the case of a single
#' value (1) or a range with minimum-maximum of values (2).
#'
#' @return Always a string.
#' @keywords internal
#'
#' @examples saros.contents:::n_rng(saros.base::ex_survey, dep="b_1", indep = "x1_sex")
n_rng <- function(data, dep, indep = NULL,
                  glue_template_1 = "{n}", glue_template_2 = "[{n[1]}-{n[2]}]") {
  # Should always return a string, no matter the inputs

  current_call <- match.call()
  current_call <- current_call[!names(current_call) %in% .saros.env$ignore_args]
  args <- check_options(call = current_call,
                        ignore_args = .saros.env$ignore_args,
                        defaults_env = makeme_global_settings_get())

  deps <- as.character(unique(dep))
  deps <- deps[!is.na(deps)]
  indeps <- as.character(unique(indep))
  indeps <- indeps[!is.na(indeps)]


  n <-
    lapply(deps, function(d) {
      if(length(indeps)>0) {
        lapply(indeps, function(i) {

          out <-
          dplyr::filter(data, dplyr::if_all(.cols = tidyselect::all_of(c(d, i)),
                                            .fns = ~!is.na(.x))) |>
            nrow()
          return(out)

        }) |> unlist()

      } else {

        out <-
          dplyr::filter(data, dplyr::if_all(.cols = tidyselect::all_of(c(d)),
                                          .fns = ~!is.na(.x))) |>
          nrow()
        return(out)

      }
    }) |>
    unlist()

  if(!is.null(n)) {
    n <-
      range(n, na.rm = TRUE) |>
      unique()

    template <-
      if(length(n)==1 && rlang::is_string(glue_template_1)) {
        glue_template_1
      } else if(rlang::is_string(glue_template_2)) {
        glue_template_2
      } else ""

    if(all(is.na(n))) {
      n <- 0
    }

    n <- glue::glue(template)

  } else n <- ""
  n
}

#' Tidyselect-version of [n_rng()]
#'
#' @inheritParams n_rng
#' @return String.
#' @export
#'
#' @examples n_range(data=saros.base::ex_survey, dep=b_1:b_3, indep=x1_sex)
n_range <- function(data, dep, indep=NULL,
                    glue_template_1 = "{n}", glue_template_2 = "[{n[1]}-{n[2]}]") {
  current_call <- match.call()
  current_call <- current_call[!names(current_call) %in% .saros.env$ignore_args]
  dep_enq <- rlang::enquo(arg = dep)
  dep_pos <- tidyselect::eval_select(dep_enq, data = data)
  indep_enq <- rlang::enquo(arg = indep)
  indep_pos <- tidyselect::eval_select(indep_enq, data = data)

  args <- check_options(call = current_call,
                        ignore_args = .saros.env$ignore_args,
                        defaults_env = makeme_global_settings_get()
  )
  args$data <- data # reinsert after check_options
  args$dep <- names(dep_pos)
  args$indep <- names(indep_pos)
  n_rng(data=data,
        dep=args$dep,
        indep=args$indep,
        glue_template_1 = glue_template_1,
        glue_template_2 = glue_template_2)
}
