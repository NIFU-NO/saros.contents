#' Embed Interactive Plot of Various Kinds Using Tidyselect Syntax
#'
#' This function allows embedding of interactive or static plots based on various types of data using tidyselect syntax for variable selection.
#'
#' @param data *Your data.frame/tibble or srvyr-object (experimental)*
#'
#'   `data.frame` // *required*
#'
#'   The data to be used for plotting.
#'
#' @param dep,indep *Variable selections*
#'
#'   <`tidyselect`> // *Default:* `NULL`, meaning everything for dep, nothing for indep.
#'
#'   Columns in `data`. `dep` is compulsory.
#'
#' @param type *Kind of output*
#'
#'   `scalar<character>` // *default:* `"prop_plot"` (`optional`)
#'
#' \describe{
#' \item{"1"}{Choose one of various plot/table kinds. Suffix indicates "html" (interactive), "docx" (MS Chart/Word tables), "pdf" (static figures/tables)}
#' \item{"2"}{Create your own makeme-method and register it. It will be served all the arguments as for }
#' }
#' For a list of registered types in your session, use `methods(makeme)`.
#'
#' @param categories_treated_as_na *NA categories*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Categories that should be treated as NA.
#'
#'
#' @param data_label *Data label*
#'
#'   `scalar<character>` // *default:* `"proportion"` (`optional`)
#'
#'   One of "proportion", "percentage", "percentage_bare", "count", "mean", or "median".
#'
#' @param plot_height *DOCX-setting*
#'
#'   `scalar<numeric>` // *default:* `12` (`optional`)
#'
#'   DOCX plots need a height, which currently cannot be set easily with a Quarto chunk option.
#'
#' @param main_font_size,label_font_size,strip_font_size,legend_font_size *Font sizes*
#'
#'   `scalar<integer>` // *default:* `6` (`optional`)
#'
#'   ONLY FOR DOCX-OUTPUT. Other output is adjusted using e.g. ggplot2::theme() or set with a global theme (ggplot2::theme_set()).
#'   Font sizes for general text (6), data label text (3), strip text (6) and legend text (6).
#'
#' @param font_family *Font family*
#'
#'   `scalar<character>` // *default:* `"sans"` (`optional`)
#'
#'   Word font family. See officer::fp_text.
#'
#' @param docx_template *Filename or rdocx object*
#'
#'   `scalar<character>|<rdocx>-object` // *default:* `NULL` (`optional`)
#'
#'   Can be either a valid character path to a reference Word file, or an existing rdocx-object in memory.
#'
#' @param return_raw *NOT IN USE*
#'
#'   `scalar<integer>` // *default:* `FALSE`
#'
#'   Whether to return the raw static element.
#'
#' @param mesos_var *Variable in `data` indicating groups to tailor reports for*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Column name in data indicating the groups for which mesos reports will be produced.
#'
#' @param mesos_group
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   String, target group.
#'
#' @param show_for *Which groups to display results for*
#'
#'   `vector<character>` // *default:* `c("target", "others", "all")` (`optional`)
#'
#'   Choose whether to produce results for target (mesos) group, others, or all combined.
#'
#' @param showNA *Show NA categories*
#'
#'   `vector<character>` // *default:* `c("ifany", "always", "never")` (`optional`)
#'
#'   Choose whether to show NA categories in the results.
#'
#' @param html_interactive *Toggle interactive plot*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Whether the plot is to be interactive (ggiraph) or static (ggplot2).
#'
#' @param hide_axis_text_if_single_variable *Hide y-axis text if just a single variable*
#'
#'   `scalar<boolean>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to hide text on the y-axis label if just a single variable.
#'
#' @param hide_chr_for_others *Hide open response displays for others*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   For mesos reports using the element "chr_table", open responses are
#'   displayed for also the entire sample (`FALSE`) or only for the mesos
#'   group to ensure data privacy (`TRUE`).
#'
#' @param hide_variable_if_all_na *Hide variable from outputs if containing all NA*
#'
#'   `scalar<boolean>` // *default:* `TRUE` (`optional`)
#'
#'   Whether to remove all variables (in particular useful for mesos) if all values are NA
#'
#' @param label_separator *How to separate main question from sub-question*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Separator for main question from sub-question.
#'
#' @param require_common_categories *Check common categories*
#'
#'   `scalar<logical>` // *default:* `NULL` (`optional`)
#'
#'   Whether to check if all items share common categories.
#'
#' @param path *Output path for DOCX*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Path to save docx-output.
#'
#' @param inverse *Flag to swap x-axis and faceting*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   If TRUE, swaps x-axis and faceting.
#'
#' @param vertical *Display plot vertically*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   If TRUE, display plot vertically.
#'
#' @param variables_always_at_top,variables_always_at_bottom *Top/bottom variables*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Column names in `data` that should always be placed at the top or bottom of figures/tables.
#'
#' @param colour_palette *Colour palette*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Must contain at least the number of unique values (including missing) in the data set.
#'
#' @param colour_2nd_binary_cat *Colour for second binary category*
#'
#'   `scalar<character>` // *default:* `"#ffffff"` (`optional`)
#'
#'   Colour for the second category in binary variables. Often useful to hide this.
#'
#' @param colour_na *Colour for NA category*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Colour as a single string for NA values, if showNA is "ifany" or "always".
#'
#' @param x_axis_label_width,strip_width *Label width of x-axis and strip texts in plots*
#'
#'   `scalar<integer>` // *default:* `20` (`optional`)
#'
#'   Width of the labels used for the categorical column names in x-axis texts and strip texts.
#'
#' @param translations *Localize your output*
#'
#'   `list<character>`
#'
#'   A list of translations where the name is the code and the value is the translation. See the examples.
#'
#' @param totals *Include totals*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to include totals in the output.
#'
#' @param digits *Decimal places*
#'
#'   `scalar<integer>` // *default:* `0L` (`optional`)
#'
#'   Number of decimal places.
#'
#' @param data_label_decimal_symbol *Decimal symbol*
#'
#'   `scalar<character>` // *default:* `"."` (`optional`)
#'
#'   Decimal marker, some might prefer a comma ',' or something else entirely.
#'
#' @param hide_label_if_prop_below *Hide label threshold*
#'
#'   `scalar<numeric>` // *default:* `NULL` (`optional`)
#'
#'   Whether to hide label if below this value.
#'
#' @param sort_by *What to sort output by*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Sort output (and collapse if requested).
#'
#' \describe{
#' \item{".top"}{The proportion for the highest category available in the variable.}
#' \item{".upper"}{The sum of the proportions for the categories above the middle category.}
#' \item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
#' \item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
#' \item{".lower"}{The sum of the proportions for the categories below the middle category.}
#' \item{".bottom"}{The proportions for the lowest category available in the variable.}
#' \item{".variable_label"}{Sort by the variable labels.}
#' \item{".id"}{Sort by the variable names.}
#' \item{".by_group"}{The groups of the by argument.}
#' \item{character()}{Character vector of category labels to sum together.}
#' }
#'
#' @param descend *Sorting order*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Reverse sorting of `sort_by` in figures and tables. See `arrange_section_by`
#'   for sorting of report sections.
#'
#' @param table_main_question_as_header *Table main question as header*
#'
#'   `scalar<logical>` // *default:* `FALSE` (`optional`)
#'
#'   Whether to include the main question as a header in the table.
#' @param ... *Dynamic dots*
#'
#'   <[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)>
#'
#'   Arguments forwarded to the corresponding functions that create the elements.
#'
#'
#'
#' @return ggplot
#' @importFrom rlang !!!
#' @export
#'
#' @examples
#' sarosmake(data = saros.base::ex_survey, dep = b_1:b_3)
#' sarosmake(data = saros.base::ex_survey, dep = b_1, indep = x1_sex)
#' sarosmake(data = saros.base::ex_survey, dep = b_1, indep = x1_sex, type = "cat_prop_plot_docx")
sarosmake <-
  function(data,
           dep = tidyselect::everything(),
           indep = NULL,
           type = c("cat_prop_plot_html",
                    "cat_freq_plot_html",
                    "int_plot_html",
                    "cat_prop_table_html",
                    "cat_freq_table_html",
                    "int_table_html",
                    "sigtest_table_html",

                    "cat_prop_plot_docx",
                    "cat_freq_plot_docx",
                    "int_plot_docx",
                    "cat_prop_table_docx",
                    "cat_freq_table_docx",
                    "int_table_docx",
                    "sigtest_table_docx"),
           ...,

           require_common_categories = TRUE,
           path = NULL,

           # Multiple output, splits
           show_for = c("all"), #"target", "others",
           mesos_var = NULL,
           mesos_group = NULL,

           totals = FALSE,
           categories_treated_as_na = NULL,
           label_separator = " - ",
           showNA = c("ifany", "always", "never"),
           data_label = c("percentage_bare", "percentage", "proportion", "count"),
           html_interactive = TRUE,
           hide_axis_text_if_single_variable = TRUE,
           hide_label_if_prop_below = .01,
           hide_chr_for_others = TRUE,
           hide_variable_if_all_na = TRUE,
           inverse = FALSE,
           vertical = FALSE,
           digits = 0,
           data_label_decimal_symbol = ".",
           x_axis_label_width = 25,
           strip_width = 25,

           sort_by = ".upper",
           descend = TRUE,
           variables_always_at_top = NULL,
           variables_always_at_bottom = NULL,


           colour_palette = NULL,
           colour_2nd_binary_cat = "#ffffff",
           colour_na = "grey",

           # Only for docx, for ggplot2 it is set globally
           plot_height = 15,
           label_font_size = 6,
           main_font_size = 6,
           strip_font_size = 6,
           legend_font_size = 6,
           font_family = "sans",

           docx_template = NULL,
           return_raw = FALSE,

           table_main_question_as_header = FALSE,

           translations =
             list(last_sep = " and ",
                  by_breakdown = " by ",
                  table_heading_N = "Total (N)",
                  by_total = "Everyone",
                  sigtest_prefix = "Significance testing of ",
                  sigtest_suffix = "",
                  mesos_group_prefix = " Group: ",
                  mesos_group_suffix = "",
                  mesos_label_all_others = "Others"
             )
           ) {


    ##

    current_call <- match.call()
    current_call <- current_call[!names(current_call) %in% .saros.env$ignore_args]
    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data)

    args <- compare_and_replace_args(call = current_call,
                                     ignore_args = .saros.env$ignore_args,
                                     defaults_env = sarosmake_global_settings_get()
                                     )
    args$data <- data # reinsert after compare_and_replace_args
    args$dep <- names(dep_pos)
    args$indep <- names(indep_pos)
    args$show_for <- args$show_for
    args$showNA <- args$showNA[1]
    args$data_label <- args$data_label[1]
    args$type <- eval(args$type)[1]

    validate_sarosmake_options(params = args)


    check_multiple_indep(data, indep = {{ indep }})


    check_category_pairs(data = data, cols_pos = c(dep_pos))

    if(grepl(x=args$type, pattern = "freq")) args$data_label <- "count"


    args$show_for |>
      rlang::set_names() |>
    lapply(FUN = function(s) {
      if(s == "target") {
        if(!rlang::is_string(args$mesos_var) || !rlang::is_string(args$mesos_group)) {
          cli::cli_abort("{.arg mesos_var} and {.arg mesos_group} must be specified (as strings) when {.arg show_for} is set as {.val {s}}.")
        }
        args$data <-
          args$data |>
          dplyr::filter(.data[[args$mesos_var]] == args$mesos_group)
      } else if(s == "others") {
        if(!rlang::is_string(args$mesos_var) || !rlang::is_string(args$mesos_group)) {
          cli::cli_abort("{.arg mesos_var} and {.arg mesos_group} must be specified (as strings) when {.arg show_for} is set as {.val {s}}.")
        }

        args$data <-
          args$data |>
          dplyr::filter(.data[[args$mesos_var]] != args$mesos_group)
      }

      args$data_summary <-
        rlang::exec(summarize_data, !!!args) |>
        post_process_sarosmake_data(data = _,
                                    indep = args$indep,
                                    showNA = args$showNA,
                                    colour_2nd_binary_cat = args$colour_2nd_binary_cat)

      rlang::exec(makeme, type=args$type, !!!args[!names(args) %in% c("type")])

    })

  }
