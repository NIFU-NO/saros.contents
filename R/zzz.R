if(!exists(".saros.env")) .saros.env <- NULL

.onLoad <- function(libname, pkgname) {
  # Initialize the .saros.env environment if not already set
  if (!exists(".saros.env")) .saros.env <<- new.env(parent = emptyenv())

  .saros.env$summary_data_sort1 <<-
    c(".top", ".upper", ".mid_upper", ".lower", ".mid_lower", ".bottom")
  .saros.env$summary_data_sort2 <<-
    c(".variable_name", ".category",
      ".count", ".count_se",
      ".count_total",
      ".proportion", ".proportion_se",
      ".mean", ".mean_se", #".mean_base",
      ".variable_label",  ".data_label", ".comb_categories",
      ".sum_value",
      ".element_name")
  .saros.env$data_label_opts <<-
    c("proportion", "percentage", "percentage_bare",
      "count", "mean", "median")

  .saros.env$ignore_args <<- c("data", "dep", "indep",
                               #"type",
                               "chapter_overview", "path", "...")

  .saros.env$sarosmake_defaults <<-
    lapply(formals(sarosmake)[!names(formals(sarosmake)) %in% c("data", "dep", "indep", "...")],
           eval)
  .saros.env$sarosmake_defaults$type <<- .saros.env$sarosmake_defaults$type[1]
  .saros.env$sarosmake_defaults$showNA <<- .saros.env$sarosmake_defaults$showNA[1]
  .saros.env$sarosmake_defaults$data_label <<- .saros.env$sarosmake_defaults$data_label[1]


  .saros.env$n_rng_defaults <<-
    lapply(formals(n_rng)[!names(formals(n_rng)) %in% c("data", "dep", "indep", "...")],
           eval)

  # Initialize global options with the factory defaults if not already set
  .saros_options <- getOption("saros", list())
  if (is.null(.saros_options$sarosmake_defaults)) {
    .saros_options$sarosmake_defaults <- .saros.env$sarosmake_defaults
    .saros_options$n_rng_defaults <- .saros.env$n_rng_defaults
    options(saros = .saros_options)
  }
}
