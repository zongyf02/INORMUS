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
      mutate(
        across(c(3:7, 9:13), parse_int),
        region = parse_region(studyid),
        site = parse_site(studyid)
      ) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
}

#' Check for missing/extraneous textfields in form2.1
#'
#' othoccup and p2q5 should be NA iff occup is not 13 (other)
#'
#' @param form form2.1
#' @return problematic rows of form2.1
#' @export
#' @import tidyverse
check_form2.1_textfield <- function(form2.1) {
  problems <- filter(form2.1,
                     (occup == 13 && (is.na(othoccup) || is.na(p2q5)))
                     || (occup != 13 && (!(is.na(othoccup) && is.na(p2q5)))))

  return(problems)
}
