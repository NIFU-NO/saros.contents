#' @export
make_content.cat_freq_plot_html <-
  function(...) {

    dots <- rlang::list2(...)

    data <- dots$data_summary

    multi <- length(dots$colour_palette) > 2

    indep_vars <- colnames(data)[!colnames(data) %in%
                                   .saros.env$summary_data_sort2]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
      length(indep_vars) == 0 &&
      dplyr::n_distinct(data$.variable_label) == 1


    max_nchar_cat <- max(nchar(levels(data$.category)), na.rm = TRUE)

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")
    x <- if(length(indep_vars) == 1 && isFALSE(dots$inverse)) indep_vars else ".variable_label"

    p <-
      dplyr::mutate(data,
                    .id = seq_len(nrow(data)),
                    .tooltip = # Tooltip contains all data except variable name
                      sprintf(fmt = stringi::stri_c("%s",
                                                    "n = %.0f",
                                                    stringi::stri_c("P = %.", dots$digits, "f%%", ignore_null=TRUE),
                                                    "%s",
                                                    "N (valid) = %.0f",
                                                    sep="\n", ignore_null = TRUE),
                              .data$.category,
                              .data$.count,
                              .data$.proportion*100,
                              .data$.variable_label,
                              .data$.count_total),
                    .tooltip = ifelse(!is.na(.data$.tooltip) & rlang::is_string(indep_vars),
                                      yes = sprintf(fmt = stringi::stri_c("%s", "%s", sep="\n", ignore_null = TRUE),
                                                    .data$.tooltip,
                                                    .data[[indep_vars]]),
                                      no = .data$.tooltip),
                    .onclick = sprintf(fmt = stringi::stri_c("%s", "Variable: %s", sep="\n", ignore_null = TRUE),
                                       .data$.tooltip, .data$.variable_name),
                    .onclick = paste0('alert(\"', .data$.onclick, '\");'),
                    .onclick = stringi::stri_replace_all_regex(.data$.onclick,
                                                               pattern = "\n",
                                                               replacement = "\\\\n")
      ) |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data$.count,
          x = .data[[x]],
          fill = .data$.category,
          group = .data$.category,
          label = .data$.data_label,
          data_id = .data$.id,
          onclick = .data$.onclick
        ),
        cumulative = TRUE) +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(tooltip = .data$.tooltip), # BUG: Messes up order of categories if enabled.
        position = ggplot2::position_dodge(width = .9),
        na.rm = TRUE,
        show.legend = TRUE
      ) +
      ggiraph::geom_text_interactive(
        mapping = ggplot2::aes(colour =
                                 ggplot2::after_scale(x = hex_bw(.data$fill))),
        position = ggplot2::position_dodge(width = .9), hjust = 2,
        # size = dots$label_font_size,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::scale_y_continuous(limits = c(-.003, NA),
                                  expand = c(0,0.03)) +
      ggiraph::scale_fill_discrete_interactive(
        name="",
        # values = dots$colour_palette,
        data_id = function(x) x,
        tooltip = function(x) x,
        drop = FALSE) +
      ggiraph::scale_colour_discrete_interactive(
        guide = FALSE,
        # values = c("black", "white"),
        drop = FALSE) +
      ggplot2::scale_x_discrete(limits = rev, labels = function(x) string_wrap(x, width = dots$x_axis_label_width)) +
      ggplot2::guides(fill = ggiraph::guide_legend_interactive(data_id="fill.guide",
                                                               byrow = TRUE,
                                                               nrow = max(c(ceiling(length(dots$colour_palette) / 5),
                                                                            (max_nchar_cat > 10)+1), na.rm = TRUE)),
                      colour = "none")

    if (length(indep_vars) > 1L ||
        (length(indep_vars) >= 1L &&
         (dplyr::n_distinct(data$.variable_label) > 1 ||
          (dplyr::n_distinct(data$.variable_label) == 1 &&
           isFALSE(dots$hide_axis_text_if_single_variable))))) {
      if(!dots$inverse) {

        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data$.variable_label),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(
                data_id = .data$.variable_label,
                tooltip = .data$.variable_label,
                label = string_wrap(.data$.label,
                                    width = dots$x_axis_label_width))),
            interactive_on = "text",
            switch = "y", scales = "free_y", space = "free_y"
          )
      } else {
        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data[[indep_vars]]),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(
                data_id = .data[[indep_vars]],
                tooltip = .data[[indep_vars]],
                label = string_wrap(.data$.label,
                                    width = dots$x_axis_label_width))),
            interactive_on = "text",
            switch = "y", scales = "free_y", space = "free_y"
          )
      }
    }

    if(!dots$vertical) {
      p + ggplot2::coord_flip()
    } else p
  }
