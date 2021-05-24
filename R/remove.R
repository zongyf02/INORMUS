#' Determines if all values produces the same output when passed
#' to a comparator 
#'
#' @param form any form 
#' @param values column names for values to be compared
#' @param comp comparator function (is.na by default unless otherwise specified)
#' 
#' @return TRUE if all comparisons pass, otherwise returns FALSE
#' 
is_all <- function(form, values, comp = is.na) {
  all <- TRUE
  for (value in values) {
    all <- (all & comp(form[value]))
  }
  
  return (all)
}

#' Counts all the items that return TRUE by the comparator 
#'
#' @param form any form 
#' @param values column names for values to be compared
#' @param comp comparator function
#' 
#' @return total number of values that satisfy the comp conditions
#' 
count_all <- function (form, values, comp) {
  sum <- 0
  for (value in values) {
    sum <- (sum + comp(form[value]))
  }
  return (sum)
}

#' Determines if all coding boxes in a row have NA values
#'
#' @param form any form 
#' @param coding a vector of column names for coding boxes
#' 
#' @return TRUE if all coding boxes are NA, otherwise FALSE
#' 
is_na_coding <- function(form, coding) {
  return (is_all(form, coding))
}

#' Determines if val is zero
#'
#' @param val numeric value or NA
#' 
#' @return TRUE if val is equal to zero
#' 
is_zero <- function(val) {
  return (val == 0)  
}

#' Determines if val is positive
#'
#' @param val numeric value or NA
#' 
#' @return TRUE if val is greater than zero
#' 
gt_zero <- function(val) {
  return (val > 0)
}

#' Determines whether a row is valid or not (Type I) 
#'
#' @param form any form 
#' @param other_val int value corresponding to the 'other' option
#' @param other string name of the 'other' column
#' @param text_box string name of the 'text_box' column
#' @param coding a vector of column names for coding boxes
#' 
#' @return TRUE if a Type I invalid case has been found, otherwise returns FALSE
#' 
#' @import tidyverse
check_invalid_I <- function(form, other_val, other, text_box, coding) {
  return ((form[other] == other_val & (is.na(form[text_box]) | is_na_coding(form, coding)))
          | (form[other] != other_val & form[other] >= 0 & 
               (!(is.na(form[text_box]) & is_na_coding(form, coding)))))
}

#' Determines whether a row is valid or not (Type II) 
#'
#' @param form any form 
#' @param other_val int value corresponding to the 'other' option
#' @param other string name of the 'other' column
#' @param text_box string name of the 'text_box' column
#' @param coding a vector of column names for coding boxes
#' @param none column name of the 'none' check box
#' @param normal any normal check boxes
#' 
#' @return TRUE for a Type II invalid case, otherwise returns FALSE
#' 
#' @import tidyverse
check_invalid_II <- function(form, other_val, other,text_box, coding, 
                             none, normal) {
  
  return (form[none] == 1 & (!is_all(form, append(normal, other), is_zero) | !is_na_coding(form, coding))) | 
    (check_invalid_I(form, other_val, other, text_box, coding))
}

#' Determines whether a row is valid or not (Type III)
#'
#' @param form any form 
#' @param groups each group contains a column name for the header check box
#' and the column names of its content elements
#' 
#' @return TRUE for a Type III invalid case, otherwise returns FALSE
#' 
#' @import tidyverse
check_invalid_III <- function(form, groups) {
  all <- FALSE
  for (i in 1:length(groups)) {
    all <- all | ((form[groups[[i]][1]] == 1) & (check_invalid_I(form, groups[[i]][2], groups[[i]][3], 
                                                                 groups[[i]][4], groups[[i]][5])))
  }
  return (all)
}

#' Filters out invalid rows in the form (Type I) 
#'
#' @param form any form 
#' @param other_val int value corresponding to the 'other' option
#' @param other string name of the 'other' column
#' @param text_box string name of the 'text_box' column
#' @param coding a vector of column names for coding boxes
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form_box_I <- function(form, other_val, other, text_box, coding) {
  
  # filters out all invalid rows from the form
  problems <- filter(form, (form[other_val] <= 0 ) | check_invalid_I(form, other_val, other, 
                                                                     text_box, coding))
  
  # returns a table of all invalid cases
  return (problems)
}

#' Filters out invalid rows in the form (Type II) 
#'
#' @param form any form 
#' @param other_val int value corresponding to the 'other' option
#' @param other string name of the 'other' column
#' @param text_box string name of the 'text_box' column
#' @param coding a vector of column names for coding boxes
#' @param none column name of the 'none' check box
#' @param normal any normal check boxes
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form_box_II <- function(form, other_val, other, text_box, coding,
                              none, normal) {
  problems <- filter(form, check_invalid_II(form, other_val, other, text_box, 
                                            coding, none, normal))
  return (problems)
}

#' Filters out invalid rows in the form (Type III) 
#' 
#' @param form any form 
#' @param groups each group contains a column name for the header check box
#' and the column names of its content elements
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
check_form_box_III <- function(form, groups, types) {
  problems <- filter(form,(count_all(form, groups, gt_zero) > 1) | 
                       check_invalid_III(form, groups, types))
  return (problems)
}