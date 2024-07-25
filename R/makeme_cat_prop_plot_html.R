#' @export
make_content.cat_prop_plot_html <-
  function(type,
           ...) {


    dots <- rlang::list2(...)

    data <- dots$data_summary

    multi <- length(dots$colour_palette) > 2

    checkbox <-
      length(unname(dots$colour_palette)[!is.na(unname(dots$colour_palette))]) == 1 && # Contains a single colour
      length(unname(dots$colour_palette)[is.na(unname(dots$colour_palette))]) == 1 # Contains a NA

    if(checkbox) {
      na_category <- names(dots$colour_palette)[is.na(unname(dots$colour_palette))]
      data$.category <- forcats::fct_relevel(data$.category,
                                             na_category,
                                             after = 1)
      dots$colour_palette[is.na(unname(dots$colour_palette))] <- "#ffffff"
      data$.data_label <- ifelse(data$.category == na_category,
                                 NA_character_, data$.data_label)
    }
    if(length(dots$colour_palette) > 0 && length(names(dots$colour_palette))==0) {
      dots$colour_palette <- stats::setNames(dots$colour_palette, levels(data$.category))
    }

    indep_vars <- colnames(data)[!colnames(data) %in%
                                   .saros.env$summary_data_sort2]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
      length(indep_vars) == 0 &&
      dplyr::n_distinct(data$.variable_label) == 1

    hide_legend <-
      checkbox

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
                              .data$.proportion * 100,
                              .data$.variable_label,
                              .data$.count_total),
                    .tooltip = ifelse(!is.na(.data$.tooltip) & rlang::is_string(indep_vars),
                                      yes = sprintf(fmt = stringi::stri_c("%s", "%s", sep="\n", ignore_null = TRUE),
                                                    .data$.tooltip,
                                                    .data[[indep_vars]]),
                                      no = .data$.tooltip),
                    .onclick = sprintf(fmt = stringi::stri_c("%s", "Variable: %s", sep="\n", ignore_null = TRUE),
                                       .data$.tooltip, .data$.variable_name),
                    .onclick = paste0('alert(\"', .data[['.onclick']], '\");'),
                    .onclick = stringi::stri_replace_all_regex(.data$.onclick,
                                                               pattern = "\n",
                                                               replacement = "\\\\n"),
                    .alpha = if(length(dots$colour_palette)>0) ifelse(.data$.category == names(dots$colour_palette)[2] & checkbox, 0, 1)
      ) |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[if (prop_family) ".proportion" else stringi::stri_c(".", dots$data_label, ignore_null=TRUE)]],
          x = .data[[x]],
          fill = .data$.category,
          # alpha = .data$.alpha,
          group = .data$.category,
          label = .data$.data_label,
          data_id = .data$.id,
          onclick = .data$.onclick
        ),
        cumulative = TRUE
      ) +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(tooltip = .data$.tooltip), # BUG: Messes up order of categories if enabled.
        position = ggplot2::position_stack(reverse = TRUE),
        na.rm = TRUE,
        show.legend = TRUE
      ) +
      ggiraph::geom_text_interactive(
        mapping = ggplot2::aes(
          colour = ggplot2::after_scale(x = hex_bw(.data$fill))),
        position = ggplot2::position_stack(vjust = .5, reverse = TRUE),
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-.003, if (prop_family) 1.015 else NA),
        expand = c(0, 0.03),
        labels = if (percentage) function(x) stringi::stri_c(ignore_null=TRUE, x * 100, "%") else ggplot2::waiver()
      ) +
      ggiraph::scale_fill_discrete_interactive(
        name = "",
        # values = dots$colour_palette,
        data_id = function(x) x,
        tooltip = function(x) x,
        drop = FALSE
      ) +
      ggiraph::scale_colour_discrete_interactive(
        guide = FALSE, #values = c("black", "white"),
        drop = FALSE) +
      ggplot2::scale_x_discrete(limits = rev, labels = function(x) string_wrap(x, width = dots$x_axis_label_width)) +
      ggplot2::guides(
        alpha = "none",
        fill = if (hide_legend) "none" else ggiraph::guide_legend_interactive(data_id = "fill.guide",
                                                                              byrow = TRUE,
                                                                              nrow = max(c(ceiling(length(dots$colour_palette) / 5),
                                                                                           (max_nchar_cat > 10)+1), na.rm = TRUE)),
        colour = "none"
      ) #+
    # ggplot2::theme_classic() +
    # ggplot2::theme(
    #   text = ggplot2::element_text(family = dots$font_family, size = dots$main_font_size),
    #   axis.text.x = ggiraph::element_text_interactive(size = dots$main_font_size),
    #   axis.text.y = if (hide_axis_text) ggplot2::element_blank() else ggiraph::element_text_interactive(data_id = "axis.text.y", size = dots$main_font_size),
    #   plot.caption = ggiraph::element_text_interactive(data_id = "plot.caption"),
    #   legend.location = "plot",
    #   legend.position = "bottom",
    #   legend.justification = "left", #if(!rlang::is_string(indep_vars)) c(-.15, 0) else c(-.35, 0),
    #   legend.direction = "horizontal",
    #   legend.key.size = ggplot2::unit(4, "mm"),
    #   legend.text = ggiraph::element_text_interactive(data_id = "legend.text", size = dots$legend_font_size),
    #   strip.placement = "outside",
    #   strip.text.x = ggplot2::element_text(margin = ggplot2::margin(l = 0, t = 0, r = 0, b = 2, "cm"), size = dots$strip_font_size),
    #   strip.text.y.left = ggiraph::element_text_interactive(data_id = "strip.text",
    #                                                          angle = dots$strip_angle,
    #                                                          hjust = 0,
    #                                                          colour = "grey20",
    #                                                          size = dots$strip_font_size), # if(length(indep_vars)>0) ggplot2::element_blank() else
    #   strip.text.y.right = ggiraph::element_text_interactive(data_id = "strip.text",
    #                                                         angle = dots$strip_angle,
    #                                                         hjust = 0,
    #                                                         colour = "grey20",
    #                                                         size = dots$strip_font_size), # if(length(indep_vars)>0) ggplot2::element_blank() else
    #   strip.background = ggiraph::element_rect_interactive(colour = NA)
    # ) +
    # ggplot2::labs(x = NULL, y = NULL)

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
                label = string_wrap(.data$.variable_label,
                                    width = dots$x_axis_label_width
                )
              )
            ),
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
                label = string_wrap(.data[[indep_vars]],
                                    width = dots$x_axis_label_width
                )
              )
            ),
            interactive_on = "text",
            switch = "y", scales = "free_y", space = "free_y"
          )
      }
    }

    if (!dots$vertical) {
      p + ggplot2::coord_flip()
    } else {
      p
    }
  }

