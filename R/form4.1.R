#' Read in form4.1
#'
#' Read in form4.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form4.1 dataframe
#' @export
#' @import tidyverse
read_form4.1 <- function(path, raw = FALSE){
  form <- read_delim(path, delim = "|",
                     col_types = "------iccccccccccccccccccccccccc----")

  if(!raw){
    form <- form %>%
      mutate(across(c(4, 6, 8:11, 13:26), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid))%>%
      relocate(c(region, site), .before = studyid)

  }
  return(form)
}
