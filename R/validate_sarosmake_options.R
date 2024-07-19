validate_sarosmake_options <- function(params) {

  unwanted_args <- names(params)[!names(params) %in% c(names(formals(sarosmake)))]
  if(length(unwanted_args) > 0) cli::cli_abort("{.arg {unwanted_args}} are not recognized valid arguments.")

  env <- lapply(formals(sarosmake)[!names(formals(sarosmake)) %in% .saros.env$ignore_args], eval)
  check_and_set_default <- function(target,
                                    param_name,
                                    validation_fun) {

    if (!validation_fun(target[[param_name]])) {
      default <- env[[param_name]]
      cli::cli_warn(paste0("{.arg {param_name}} is invalid (it is {.obj_type_friendly {target[[param_name]]}}, and specified as {target[[param_name]]}). Using default: {default}"))
      default
    } else target[[param_name]]
  }
  is_scalar_finite_doubleish <- function(x) {
    is.numeric(x) && length(x) == 1 && is.finite(x)
  }
  is_bool <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)


  arg_params <-
    list(
      # Data frames
      data = list(fun = function(x) inherits(x, "data.frame") || inherits(x, "survey")),

      # Character vectors (not enums)
      type = list(fun = function(x) rlang::is_string(x)),
      mesos_var = list(fun = function(x) is.null(x) || rlang::is_string(x)),
      path = list(fun = function(x) is.null(x) || rlang::is_string(x)),
      label_separator = list(fun = function(x) is.null(x) || is.character(x)),
      variables_always_at_top = list(fun = function(x) is.null(x) || (is.character(x) && all(x %in% colnames(params$data)))),
      variables_always_at_bottom = list(fun = function(x) is.null(x) || (is.character(x) && all(x %in% colnames(params$data)))),
      font_family = list(fun = function(x) rlang::is_string(x)),
      data_label_decimal_symbol = list(fun = function(x) rlang::is_string(x)),

      # Boolean
      require_common_categories = list(fun = is_bool),
      return_raw = list(fun = is_bool),
      descend = list(fun = is_bool),
      vertical = list(fun = is_bool),
      hide_chr_for_others = list(fun = is_bool),
      hide_variable_if_all_na = list(fun = is_bool),
      hide_axis_text_if_single_variable = list(fun = is_bool),
      totals = list(fun = is_bool),
      table_main_question_as_header = list(fun = is_bool),

      # Numeric and integer
      hide_label_if_prop_below = list(fun = function(x) is_scalar_finite_doubleish(x) && x >= 0 && x <= 1),
      digits = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0),
      label_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      main_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      strip_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      legend_font_size = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 72),
      strip_width = list(fun = function(x) rlang::is_integerish(x, n = 1, finite = TRUE) && x >= 0 && x <= 200),
      plot_height = list(fun = function(x) rlang::is_double(x, n = 1, finite = TRUE) && x >= 0 && x <= 200),

      # Enums
      data_label = list(fun = function(x) is.character(x) && any(env$data_label == x[1])),
      showNA = list(fun = function(x) is.character(x) && any(env$showNA == x[1])),
      colour_palette_nominal = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)),
      colour_palette_ordinal = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)),
      colour_na = list(fun = function(x) (is.character(x) && all(is_colour(x))) || is.null(x) || is.function(x)),

      # List
      translations = list(fun = function(x) rlang::is_bare_list(x) && all(unlist(lapply(x, function(.x) is.character(.x))))) ### SHOULD BE MORE SPECIFIC FOR EACH ITEM?


    )

  for(par in names(arg_params)) {

    params[[par]] <-
      check_and_set_default(target = params,
                            param_name = par,
                            validation_fun = arg_params[[par]]$fun)
  }

  params$type <- params$type[1]
  params$showNA <- params$showNA[1]
  params$data_label <- params$data_label[1]

  if(rlang::is_string(params$mesos_var)) {
    if(!any(colnames(params$data) == params$mesos_var)) {
      cli::cli_abort("{.arg mesos_var}: {.arg {params$mesos_var}} not found in data.")
    }
    if(all(is.na(params$data[[params$mesos_var]]))) {
      cli::cli_abort("{.arg mesos_var}: All mesos_var entries are NA.")
    }
  }



  params

}
