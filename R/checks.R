#' Check that consent date should be on the same day, or after the date of injury
#' 
#' condate after injdate less than 92 days
#' 
#' @param form1.1 a dataframe containing form1.1
#' @param form3.1 a dataframe containing form3.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_condate_injdate <- function(form1.1, form3.1) {
  problems <- merge_forms(
    list(select(form1.1, region, site, studyid, ptinit, condate),
         select(form3.1, region, site, studyid, ptinit, injdate))) %>%
    mutate(
      date_diff = difftime(parse_dmY(condate),
                           parse_dmY(injdate),
                           units = "days"),
      comment = "Consent date should be on the same day, or after the date of injury") %>%
    filter(condate != "N" & (is.na(date_diff) | date_diff < 0 | date_diff > 92))
  
  return(problems)
}

#' Check that hospital admission date should be on the same day,
#' or after the date of injury
#' 
#' hpsdate after injdate less than 92 days
#' 
#' @param form3.1 a dataframe containing form3.1
#' @param form4.1 a dataframe containing form4.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_hspdate_injdate <- function(form3.1, form4.1) {
  problems <- merge_forms(
    list(select(form4.1, region, site, studyid, ptinit, hspdate),
         select(form3.1, region, site, studyid, ptinit, injdate))) %>%
    mutate(
      diff_date = difftime(parse_dmY(hspdate),
                           parse_dmY(injdate),
                           units = "days"),
      comment = "Hospital admission date should be on the same day, or after the date of injury") %>%
    filter(is.na(diff_date) | diff_date < 0 | diff_date > 92)
  
  return(problems)
}