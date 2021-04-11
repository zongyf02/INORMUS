#' Determines if all coding boxes in a row have NA values
#'
#' @param form any form 
#' @param coding a vector of column names for coding boxes
#' 
#' @return TRUE if all coding boxes are NA, otherwise FALSE
#' 
#' @import tidyverse

is_na_coding <- function(form, coding) {
  all_na <- TRUE
  for (box in coding) {
    all_na <- (all_na & is.na(form[box]))
  }
  return (all_na)
}

#' Determines whether a row is valid or not 
#'
#' @param form any form 
#' @param other_val int value corresponding to the 'other' option
#' @param other string name of the 'other' column
#' @param text_box string name of the 'text_box' column
#' @param coding a vector of column names for coding boxes
#' 
#' @return TRUE if all coding boxes are NA, otherwise FALSE
#' 
#' @import tidyverse

check_invalid <- function (form, other_val, other, text_box, coding) {
    return ((form[other] == other_val & (is.na(form[text_box]) | is_na_coding(form, coding)))
            | (form[other] != other_val & form[other] >= 0 & 
                 (!(is.na(form[text_box]) & is_na_coding(form, coding)))))
}

#' Filters out invalid rows in the form 
#'
#' @param form any form 
#' @param other_val int value corresponding to the 'other' option
#' @param other string name of the 'other' column
#' @param text_box string name of the 'text_box' column
#' @param coding a vector of column names for coding boxes
#' 
#' @return a dataframe containing all the invalid rows
#' 
#' @import tidyverse
#' @export

check_form_box <- function(form, other_val, other, text_box, coding) {
  
  # filters out all invalid rows from the form
  problems <- filter(form, check_invalid(form, other_val, other, text_box, coding))
  
  # returns a table of all invalid cases
  return (problems)
}


