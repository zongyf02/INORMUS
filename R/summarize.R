#' Format a number to a percentage
#'
#'the percentage will have 2 digits after the decimal
#'
#' @param x number
#' @return percentage as a string
#' @export
percent <- function(x) {
  paste0(
    formatC(x * 100, format = "f", digits = 2),
    "%")
}

#' Summarize form1.1
#'
#' Takes in any form containing all information of form1.1
#' For example, form1.1 merged with form2.1 will work
#' Returns a summary of the freqencies and percentages
#' of each datapoint
#'
#' @param form dataframe containing all infor of form1.1
#' @param ... grouping
#' @return dataframe, summary table
#' @export
#' @import tidyverse
summarize_form1.1 <- function(form, ...) {
  return
  form %>% group_by(...) %>%
    summarize(n = n(),
              pt18T = sum(pt18 == TRUE, na.rm = TRUE),
              pt18T_p = percent(mean(pt18 == TRUE, na.rm = TRUE)),
              pt18F = sum(pt18 == FALSE, na.rm = TRUE),
              pt18F_p = percent(mean(pt18 == FALSE, na.rm = TRUE)),
              pt18NA = sum(is.na(pt18)),
              pt18NA_p = percent(mean(is.na(pt18))),
              fracdisT = sum(fracdis == TRUE, na.rm = TRUE),
              fracdisT_p = percent(mean(fracdis == TRUE, na.rm = TRUE)),
              fracdisF = sum(fracdis == FALSE, na.rm = TRUE),
              fracdisF_p = percent(mean(fracdis == FALSE, na.rm = TRUE)),
              fracdisNA = sum(is.na(fracdis)),
              fracdisNA_p = percent(mean(is.na(fracdis))),
              acute3mT = sum(acute3m == TRUE, na.rm = TRUE),
              acute3mT_p = percent(mean(acute3m == TRUE, na.rm = TRUE)),
              acute3mF = sum(acute3m == FALSE, na.rm = TRUE),
              acute3mF_p = percent(mean(acute3m == FALSE, na.rm = TRUE)),
              acute3mNA = sum(is.na(acute3m)),
              acute3mNA_p = percent(mean(is.na(acute3m))),
              willingT = sum(willing == TRUE, na.rm = TRUE),
              willingT_p = percent(mean(willing == TRUE, na.rm = TRUE)),
              willingF = sum(willing == FALSE, na.rm = TRUE),
              willingF_p = percent(mean(willing == FALSE, na.rm = TRUE)),
              willingNA = sum(is.na(willing)),
              willingNA_p = percent(mean(is.na(willing))),
              complyT = sum(comply == TRUE, na.rm = TRUE),
              complyT_p = percent(mean(comply == TRUE, na.rm = TRUE)),
              complyF = sum(comply == FALSE, na.rm = TRUE),
              complyF_p = percent(mean(comply == FALSE, na.rm = TRUE)),
              complyNA = sum(is.na(comply)),
              complyNA_p = percent(mean(is.na(comply))),
              ptstatusT = sum(ptstatus == TRUE, na.rm = TRUE),
              ptstatusT_p = percent(mean(ptstatus == TRUE, na.rm = TRUE)),
              ptstatusF = sum(ptstatus == FALSE, na.rm = TRUE),
              ptstatusF_p = percent(mean(ptstatus == FALSE, na.rm = TRUE)),
              ptstatusNA = sum(is.na(ptstatus)),
              ptstatusNA_p = percent(mean(is.na(ptstatus))),
              condate_min = min(condate, na.rm = TRUE),
              condate_max = max(condate, na.rm = TRUE))
}

#' Same as summarize_form1.1
#'
#'An alias  to summarize_form1.1
#'
#' @param form dataframe containing all infor of form1.1
#' @param ... grouping
#' @return dataframe, summary table
#' @export
#' @import tidyverse
summarise_form1.1 <- function(form, ...) {
  summarize_form1.1(form, ...)
}
