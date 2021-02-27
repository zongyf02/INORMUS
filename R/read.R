#' Convert integer to logicals
#'
#' 0: NA
#' 1: TRUE
#' 2: FALSE
#'
#' @param x integer
#' @return NA/T/F
#' @export
#' @import tidyverse
parse_tf <- function(x) {
  case_when(
    x == 0 ~ NA,
    x == 1 ~ TRUE,
    x == 2 ~ FALSE)
}


#' Read in form1.1
#'
#' Read in form1.1 from path
#' Convert integers to logicals
#'
#' @param path path to file
#' @return form1.1 dataframe
#' @export
#' @import tidyverse
read_form1.1 <- function(path) {
  form <- suppressWarnings(read_delim(
    path, delim = "|",
    col_types = "------iciiiiiic----"))

  form <- form %>%
    mutate(across(3:8, parse_tf)) %>%
    mutate(condate = parse_date(
      condate, format = "%d/%m/%Y", na = "N"))

  return(form)
}
