#' Read in form7.2
#'
#' Read in form7.2 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form7.2 dataframe
#' @export
#' @import tidyverse
read_form7.2 <- function(path, raw = FALSE) {
  read_form7.x(path, raw, "~2")
}
