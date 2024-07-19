#' @export
makeme.cat_table_html <-
  function(...) {

    dots <- rlang::list2(...)
    data_summary <- dots$data_summary

    # check_summary_data_cols(data, call = call)

    if(dots$data_label %in% c("percentage", "percentage_bare", "proportion")) {
      data_label2 <- "count"
    } else {
      data_label2 <- "percentage"
    }




    ######### MUST TIDY UP FROM HERE ############
    main_question <-

      get_main_question(x=data_summary$.variable_label,
                        label_separator = dots$label_separator,
                         warn_multiple = TRUE) |>
      unique()

    if(length(dots$indep)>0) {
      indep_label <- saros.base::get_raw_labels(data = dots$data, col_pos = dots$indep)
      indep_label <- get_main_question(indep_label,
                                        label_separator = dots$label_separator,
                                        warn_multiple = TRUE)
      indep_label <- unique(indep_label)
      if(nchar(indep_label)==0) browser() #cli::cli_warn("Indep {.var {indep_pos}} lacks a label.")

    } else indep_label <- character(0)

    # indep_label <- unname(saros.base::get_raw_labels(data = dots$data, col_pos = dots$indep))


    cat_lvls <- levels(data_summary[[".category"]])
    cat_lvls <- cat_lvls[cat_lvls %in% unique(data_summary[[".category"]])]
    if(length(indep_label)==1 && length(names(dots$indep))==0) {
      cli::cli_abort("Something wrong in function.")
    }

    # if(any(data_summary$.variable_label == "Driving")) {
    #
    #   data_summary |>
    #     dplyr::mutate(N = sum(.data$.count, na.rm=TRUE), # Move to summarize_data?
    #                   .by = tidyselect::all_of(c(".variable_name", names(dots$indep)))) |>
    #     dplyr::arrange(as.integer(.data$.variable_label), if(length(dots$indep)>0) as.integer(.data[[names(dots$indep)]])) |>
    #     # tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(dots$indep), "N")),
    #     #                  names_from = ".category", values_from = ".data_label") |>
    #     print()
    # }
    data_out <-
      data_summary |>
      dplyr::mutate(N = sum(.data$.count, na.rm=TRUE), # Move to summarize_data?
                    .by = tidyselect::all_of(c(".variable_name", names(dots$indep)))) |>
      dplyr::arrange(as.integer(.data$.variable_label), if(length(dots$indep)>0) as.integer(.data[[names(dots$indep)]])) |>
      tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(dots$indep), "N")),
                         names_from = ".category", values_from = ".data_label") |>
      dplyr::relocate(tidyselect::all_of(c(".variable_label", names(dots$indep), cat_lvls, "N")), .after = 1) |>
      # dplyr::relocate("N", .after = tidyselect::last_col()) |>
      dplyr::rename_with(.cols = tidyselect::all_of(cat_lvls),
                         .fn = ~stringi::stri_c(ignore_null=TRUE, .x, if(dots$data_label %in% c("percentage", "percentage_bare")) " (%)")) |>
      dplyr::rename_with(.cols = "N",
                         .fn = function(x) dots$translations$table_heading_N)
    if(length(dots$indep)>0 &&
       is.character(indep_label) &&
       length(indep_label) == length(dots$indep) &&
       all(nchar(indep_label)>0)) {

      data_out <- dplyr::rename_with(data_out,
                                     .cols = tidyselect::all_of(names(dots$indep)),
                                     .fn = function(x) indep_label)
    }

    if(rlang::is_string(main_question) && stringi::stri_length(main_question)>0) {
      names(data_out)[names(data_out)==".variable_label"] <- main_question
    }
    data_out[[1]] <- if(dplyr::n_distinct(data_out[[1]], na.rm = FALSE) > 1) data_out[[1]]


    ## MOVE THIS AND THE pivot_wider ABOVE TO makeme.cat_table()

    if(dots$return_raw) {
      data_out
    } else {

      dots$data_label <- data_label2

      data_out2 <-
        data_summary |>
        dplyr::mutate(N = sum(.data$.count, na.rm=TRUE),
                      .by = tidyselect::all_of(c(".variable_name", dots$indep))) |>
        dplyr::arrange(as.integer(.data$.variable_label)) |>
        tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", dots$indep, "N")),
                           names_from = ".category", values_from = ".data_label") |>
        dplyr::relocate(tidyselect::all_of(cat_lvls), .after = 1) |>
        dplyr::relocate("N", .after = tidyselect::last_col()) |>
        dplyr::rename_with(.cols = tidyselect::all_of(cat_lvls),
                           .fn = ~stringi::stri_c(ignore_null=TRUE, .x, if(dots$data_label %in% c("percentage", "percentage_bare")) " (%)")) |>
        dplyr::rename_with(.cols = "N",
                           .fn = function(x) dots$translations$table_heading_N) |>
        dplyr::rename_with(.cols = tidyselect::all_of(dots$indep), .fn = function(x) indep_label)

      if(rlang::is_string(main_question) && stringi::stri_length(main_question)>0 &&
         isTRUE(dots$table_main_question_as_header)) {
        names(data_out2)[names(data_out)==".variable_label"] <- main_question
      }

      data_out
    }
  }

