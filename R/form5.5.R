#' Read in form5.5
#'
#' Read in form5.5 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.5 dataframe
#' @export
#' @import tidyverse
read_form5.5 <- function(path, raw = FALSE) {
  read_form5.159(path, raw, "_2")
}
