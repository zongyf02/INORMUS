#' Read in form2.2
#'
#' Read in form2.2 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form2.2 dataframe
#' @export
#' @import tidyverse
read_form2.2 <- function(path, raw = FALSE) {
  form <- read_delim (file.choose(),
                      delim = "|",
                      col_types = "------iccccccccccccccccccccccc----")

  if(!raw) {
    form <- form %>%
      mutate(across(3:19, parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Check for missing/extraneous coding boxes in form2.2
#'
#' all p3q10x and comorb should be NA iff othercm is not 1 (checked)
#'
#' @param form form2.2
#' @return problematic rows of form2.2
#' @export
#' @import tidyverse
check_form2.2_box10 <- function(form2.2) {
  problems <- filter(form2.2,
                     (othercm == 1 && (is.na(comorb) ||
                        (is.na(p3q101) && is.na(p3q102) && is.na(p3q103)))) ||
                       (othercm == 0 && (!(is.na(comorb) && is.na(p3q101) &&
                                           is.na(p3q102) && is.na(p3q103)))))
  return(problems)
}
