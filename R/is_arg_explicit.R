is_arg_explicit <- function(arg_name, call) {
  # Get the actual arguments passed to the function call
  actual_args <- as.list(call)

  # Check if the argument was explicitly passed
  if (arg_name %in% names(actual_args)) {
    TRUE
  } else {
    FALSE
  }
}

# Helper function to get argument value considering priorities
get_argument_value <- function(arg_name, call, defaults_env, formals_list) {
  if (is_arg_explicit(arg_name, call)) {
    return(eval(call[[arg_name]]))
  } else if (!is.null(defaults_env[[arg_name]])) {
    return(defaults_env[[arg_name]])
  } else {
    return(formals_list[[arg_name]])
  }
}

compare_and_replace_args <- function(call,
                                     defaults_env = .saros.env$sarosmake_defaults) {
  default_values <- formals(sarosmake)
  default_values <- default_values[!names(default_values) %in% .saros.env$ignore_args]
  default_values <- lapply(default_values, eval)

  # Loop over each argument in the function
  arg_names <- names(default_values)
  final_args <- list()

  for (arg_name in arg_names) {
    final_args[[arg_name]] <- get_argument_value(arg_name = arg_name,
                                                 call = call,
                                                 defaults_env = defaults_env,
                                                 formals_list = default_values)
  }
  final_args
}
