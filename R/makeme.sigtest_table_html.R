#' @export
makeme.sigtest_table_html <-
  function(...) {

    dots <- rlang::list2(...)

    data <- dots$data

    main_question <-
      get_main_question(data$.variable_label,
                        label_separator = dots$label_separator,
                        warn_multiple = TRUE)
    main_question <- unique(main_question)
    main_question <-
      if(!is.null(dots$label_separator) &&
         length(main_question)==1 &&
         !is.na(main_question) &&
         nchar(main_question) > 0) main_question else "Variable"

    tidyr::expand_grid(y = dots$dep, x = dots$indep) |>
      dplyr::rowwise() |>
      dplyr::group_map(
        .keep = TRUE,
        .f = ~{
          if(!is.na(.x$y)) {

            if(!is.na(.x$x)) {

              data2 <- data[!is.na(data[[.x$y]]) &
                              !is.na(data[[.x$x]]), , drop=FALSE]
            } else {
              data2 <- data[!is.na(data[[.x$y]]), , drop=FALSE]
            }

            y <- data[[.x$y]]
            x <- if(!is.null(.x$x) && !is.na(.x$x)) data2[[.x$x]]
            stat_result <- find_test2(y=y, x=x)
            if(nrow(stat_result) >= 1) {
              .x[[".p_value"]] <- stat_result$.p_value
              .x[[".bi_test"]] <- stat_result$.bi_test
            }
          }

        }) |>
      dplyr::rename_with(.cols = 1, .fn = function(x) main_question)
  }



# sigtest_table <- function(data,
#                           dep,
#                           indep=NULL) {
#   if(!is.character(dep)) cli::cli_abort("{.arg dep} must be character vector.")
#
#   if(is.character(indep)) {
#
#     out <- expand.grid(dep = dep, indep = indep, stringsAsFactors = FALSE)
#     nr <- seq_len(nrow(out))
#     out2 <- lapply(nr, function(i) {
#       tryCatch({
#         find_test2(y = data[[out[i, "dep", drop=TRUE]]],
#                    x = data[[out[i, "indep", drop=TRUE]]])
#       },
#       error = function(e) data.frame(.bi_test = NA_character_, .p_value = NA_real_))
#     })
#
#   } else if(is.null(indep)) {
#
#     out <- expand.grid(dep = dep, stringsAsFactors = FALSE)
#     nr <- seq_len(nrow(out))
#     out2 <- lapply(nr, function(i) {
#       tryCatch({
#         find_test2(y = data[[out[i, "dep", drop=TRUE]]],
#                    x = NULL)
#       },
#       error = function(e) data.frame(.bi_test = NA_character_, .p_value = NA_real_))
#
#     })
#   }
#   cbind(out, do.call(rbind, out2))
#
# }
