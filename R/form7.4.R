#' Read in form7.4
#'
#' Read in form7.4 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form7.4 dataframe
#' @export
#' @import tidyverse
read_form7.4 <- function(path, raw = FALSE) {
  read_form7.x(path, raw, "_4")
}
