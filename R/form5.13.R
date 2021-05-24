#' Read in form5.13
#'
#' Read in form5.13 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.13 dataframe
#' @export
#' @import tidyverse
read_form5.13 <- function(path, raw = FALSE){
  form <- read_delim(path, delim = "|",
                     col_types = "------icccccc----")

  if(!raw){
    form <- form %>%
      mutate(across(c(3, 5:7), parse_int),
             dlevel = parse_dec(dlevel),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)

  }
  return(form)
}
