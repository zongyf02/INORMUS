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
  form <- read_delim(path, delim = "|",
                      col_types = "------iccccccccccccccccccccccc----")

  form <- rename(form, pneum_2.2 = pneum)

  if(!raw) {
    form <- form %>%
      mutate(across(c(3:19, 21:24), parse_num),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}