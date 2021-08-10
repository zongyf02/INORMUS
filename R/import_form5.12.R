#' Read in form5.12
#'
#' Read in form5.12 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.12 dataframe
#' @export
#' @import tidyverse
read_form5.12 <- function(path, raw = FALSE){
  form <- read_delim(path, delim = "|",
                     col_types = "------iccccccccccccccccccc----")

  # append "_3" to colnames
  form <- rename_with(form,
                      function(colname) {paste(colname, "_3", sep = "")},
                      .cols = 3:ncol(form))

  if(!raw){
    form <- form %>%
      mutate(across(c(3:7, 9, 11:12, 14:15, 17:20), parse_num),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}
