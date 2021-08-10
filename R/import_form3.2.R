#' Read in form3.2
#'
#' Read in form3.2 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form3.2 dataframe
#' @export
#' @import tidyverse
read_form3.2 <- function(path, raw = FALSE) {
  form <-read_delim(
    path, delim = "|",
    col_types = "------iccccccccccccccccccccccccccccccccccccccccccccccc----")

  if (!raw) {
    form <- form %>%
      mutate(across(c(3, 5:6, 8:15, 17:25, 27:35, 37:48), parse_num),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}