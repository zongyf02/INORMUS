#' Read in form6.1
#'
#' Read in form6.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form6.1 dataframe
#' @export
#' @import tidyverse
read_form6.1 <- function(path, raw = FALSE){
  form <- read_delim(path, delim = "|",
                     col_types = "------iccccccccccccccccccccccccccccccccccccccccccccc----")

  form <- rename(form, pneum_6.1 = pneum)

  if(!raw){
    form <- form %>%
      mutate(across(c(4:5, 7, 9, 11, 13:20, 22:29, 31:34, 36:37, 42:46), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid))%>%
      relocate(c(region, site), .before = studyid)
    }
  return(form)
}
