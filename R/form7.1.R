#' Read in form7.x
#'
#' Read in form7.x from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @param sfx what to append to column names
#' @return form7.x dataframe
#' @import tidyverse
read_form7.x <- function(path, raw, sfx){
  form <- read_delim(path, delim = "|",
                     col_types = "------icccccccccccccccccccccccccccccc----")
  # add sfx to colnames
  form <- rename_with(form,
                      function(colname) {paste(colname, sfx, sep = "")},
                      .cols = 3:ncol(form))

  if(!raw) {
    form <- form %>%
      mutate(across(c(5, 6, 8, 10:17, 19:21, 26:31), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Read in form7.1
#'
#' Read in form7.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form7.1 dataframe
#' @export
#' @import tidyverse
read_form7.1 <- function(path, raw = FALSE) {
  read_form7.x(path, raw, "_1")
}
