#' Read in form5.8
#'
#' Read in form5.8 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.8 dataframe
#' @export
#' @import tidyverse
read_form5.8 <- function(path, raw = FALSE) {
  read_form5.4812(path, raw, "_2")
}
