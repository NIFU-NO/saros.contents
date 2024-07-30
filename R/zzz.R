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
      ".mean", ".mean_se",
      ".variable_label", ".variable_label_prefix",
      ".data_label", ".comb_categories",
      ".sum_value")
  .saros.env$data_label_opts <<-
    c("proportion", "percentage", "percentage_bare",
      "count", "mean", "median")

  .saros.env$ignore_args <<- c("data",
                               "dep",
                               "indep",
                               "chapter_overview",
                               "chapter_structure",
                               "call",
                               "...")

  .saros.env$makeme_defaults <<-
    lapply(formals(makeme)[!names(formals(makeme)) %in% .saros.env$ignore_args],
           eval)
  .saros.env$makeme_defaults$type <<- .saros.env$makeme_defaults$type[1]
  .saros.env$makeme_defaults$showNA <<- .saros.env$makeme_defaults$showNA[1]
  .saros.env$makeme_defaults$data_label <<- .saros.env$makeme_defaults$data_label[1]

  .saros.env$make_link_defaults <<-
    lapply(formals(make_link)[!names(formals(make_link)) %in% .saros.env$ignore_args],
           eval)


  .saros.env$n_rng_defaults <<-
    lapply(formals(n_rng)[!names(formals(n_rng)) %in% .saros.env$ignore_args],
           eval)

  # Initialize global options with the factory defaults if not already set
  .saros_options <- getOption("saros", list())
  if (is.null(.saros_options$makeme_defaults)) {
    .saros_options$makeme_defaults <- .saros.env$makeme_defaults
    .saros_options$make_link_defaults <- .saros.env$make_link_defaults
    .saros_options$n_rng_defaults <- .saros.env$n_rng_defaults
    options(saros = .saros_options)
  }
}
