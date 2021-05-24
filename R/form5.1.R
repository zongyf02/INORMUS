#' Read in form5.1, 5.5, or 5.9
#'
#' Read in form5.1, 5.5, or 5.9 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @param sfx what to append to column names
#' @return form5.1, 5.5, or 5.9 dataframe
#' @import tidyverse
read_form5.159 <- function(path, raw, sfx) {
  form <-read_delim(
    path, delim = "|",
    col_types = "------iccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc----")

  # append sfx to colnames
  form <- rename_with(form,
                      function(colname) {paste(colname, sfx, sep = "")},
                      .cols = 3:ncol(form))

  if (!raw) {
    form <- form %>%
      mutate(across(c(3:31, 33:40, 42:69, 71:84, 86), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Read in form5.1
#'
#' Read in form5.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form5.1 dataframe
#' @export
#' @import tidyverse
read_form5.1 <- function(path, raw = FALSE) {
  read_form5.159(path, raw, "_1")
}
