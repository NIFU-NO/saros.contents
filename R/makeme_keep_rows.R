makeme_keep_rows <- function(data, crwd, mesos_var, mesos_group) {
  if(crwd == "target") {

    return(as.character(data[[mesos_var]]) == mesos_group)

  } else if(crwd == "others") {

    return(as.character(data[[mesos_var]]) != mesos_group)

  } else if(crwd == "all") {

    return(TRUE)
  }
}
