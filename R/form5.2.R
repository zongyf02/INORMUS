#' Read in form5.2, 5.6, or 5.10
#'
#' Read in form5.2, 5.6, or 5.10 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @param sfx what to append to column names
#' @return form5.2, 5.6, or 5.10 dataframe
#' @import tidyverse
read_form5.2610 <- function(path, raw, sfx) {
  form <-read_delim(
    path, delim = "|",
    col_types = "------iccccccccccccccccccccccccccccccccc----")

  # append sfx to colnames
  form <- rename_with(form,
                      function(colname) {paste(colname, sfx, sep = "")},
                      .cols = 3:ncol(form))

  if (!raw) {
    form <- form %>%
      mutate(across(c(3:4, 6:10, 12:19, 21:23, 25:34), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Read in form5.2
#'
#' Read in form5.2 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.2 dataframe
#' @export
#' @import tidyverse
read_form5.2 <- function(path, raw = FALSE) {
  read_form5.2610(path, raw, "_1")
}
