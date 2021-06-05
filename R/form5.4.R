#' Read in form5.4, 5.8, or 5.12
#'
#' Read in form5.4, 5.8, or 5.12 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @param sfx what to append to column names
#' @return form5.4, 5.8, or 5.12 dataframe
#' @import tidyverse
read_form5.4812 <- function(path, raw, sfx){
  form <- read_delim(path, delim = "|",
                     col_types = "------icccccccccccccccccccc----")

  # append sfx to colnames
  form <- rename_with(form,
                      function(colname) {paste(colname, sfx, sep = "")},
                      .cols = 3:ncol(form))

  if(!raw){
    form <- form %>%
      mutate(across(c(3:7, 9, 11:12, 14:15, 17:21), parse_num),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Read in form5.4
#'
#' Read in form5.4 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.4 dataframe
#' @export
#' @import tidyverse
read_form5.4 <- function(path, raw = FALSE) {
  read_form5.4812(path, raw, "_1")
}
