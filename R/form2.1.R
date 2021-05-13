#' Read in form2.1
#'
#' Read in form2.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form2.1 dataframe if raw is FALSE
#' @export
#' @import tidyverse
read_form2.1 <- function(path, raw = FALSE) {
  form <- read_delim(path, delim = "|",
                     col_types = "------icccccccccccc----")

  if (!raw) {
    form <- form %>%
      mutate(across(c(3:7, 9:13), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)
      ) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Check for missing/extraneous coding boxes in form2.1
#'
#' othoccup and p2q5 should be NA iff occup is not 13 (other)
#'
#' @param form form2.1
#' @return problematic rows of form2.1
#' @export
#' @import tidyverse
check_form2.1_box5 <- function(form2.1) {
  problems <- filter(form2.1,
                     (occup == 13 & (is.na(othoccup) | is.na(p2q5)))
                     | (occup != 13 & (!(is.na(othoccup) & is.na(p2q5)))))

  return(problems)
}

#' Filters out invalid rows for box 5 form 2.1
#' 
#' @param form form2.1 
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_from2.1_box5 <- function(form) {
  return (check_form_box_I(form, 13, "occup", "othoccup", "p2q5"))
}