#' Read in form5.6
#'
#' Read in form5.6 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.6 dataframe
#' @export
#' @import tidyverse
read_form5.6 <- function(path, raw = FALSE) {
  read_form5.2610(path, raw, "_2")
}
