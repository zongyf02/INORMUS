#' Read in form5.14
#'
#' Read in form5.14 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.14 dataframe
#' @export
#' @import tidyverse
read_form5.14 <- function(path, raw = FALSE){
  form <- read_delim(path, delim = "|",
                     col_types = "------iccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc----")

  if(!raw){
    form <- form %>%
      mutate(across(c(3:12, 14, 16:24, 26:35, 37:47, 49, 51:59, 61:70, 72:82,
                      84, 86:94, 96:105, 107:118), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }
  return(form)
}
