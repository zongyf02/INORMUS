#' Read in form1.1
#'
#' Read in form1.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form1.1 dataframe
#' @export
#' @import tidyverse
read_form1.1 <- function(path, raw = FALSE) {
  form <- read_delim(path, delim = "|",
                     col_types = "------icccccccc----")

  if (!raw) {
    form <- form %>%
      mutate(across(3:8, parse_int),
             condate = parse_dmY(condate),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}
