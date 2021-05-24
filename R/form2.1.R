#' Read in form2.1
#'
#' Read in form2.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form2.1 dataframe if raw is FALSE
#' @export
#' @import tidyverse
read_form2.1 <- function(path, raw = FALSE) {
  form <- read_delim(path, delim = "|",
                     col_types = "------icccccccccccc----")

  if (!raw) {
    form <- form %>%
      mutate(across(c(3:7, 9:13), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)
      ) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}