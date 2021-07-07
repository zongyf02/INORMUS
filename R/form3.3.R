#' Read in form3.3
#'
#' Read in form3.3 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form3.3 dataframe
#' @export
#' @import tidyverse
read_form3.3 <- function(path, raw = FALSE) {
  form <- read_delim (path, delim = "|",
                      col_types = "------icccccc----")
  
  if (!raw) {
    form <- form %>%
      mutate(across(c("p50dist", "p50city"), parse_num),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }
  
  return(form)
}