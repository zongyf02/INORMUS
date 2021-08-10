#' Read in form5.3, 5.7, or 5.11
#'
#' Read in form5.3, 5.7, or 5.11 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @param sfx what to append to column names
#' @return form5.3, 5.7, or 5.11 dataframe
#' @import tidyverse
read_form5.3711 <- function(path, raw, sfx){
  form <- read_delim(
    path, delim = "|",
    col_types = "------icccccccccccccccccccccccccccccccccccccccc----")

  # append sfx to colnames
  form <- rename_with(form,
                      function(colname) {paste(colname, sfx, sep = "")},
                      .cols = 3:ncol(form))

  if (!raw) {
    form <- form %>%
      mutate(across(c(3:23, 25:31, 33, 35:41), parse_num),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Read in form5.3
#'
#' Read in form5.3 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.3 dataframe
#' @export
#' @import tidyverse
read_form5.3 <- function(path, raw = FALSE) {
  read_form5.3711(path, raw, "_1")
}
