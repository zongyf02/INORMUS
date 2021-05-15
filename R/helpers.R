#' Convert string to integer
#'
#' M will be converted to -1
#' * (lost) will be converted to -2
#'
#' @param x string
#' @return integer
#' @export
#' @import tidyverse
parse_int <- function(x) {
  x = if_else(x == "M", "-1",
          if_else(x == "*", "-2",
                  if_else(x == "N", "-3", x)))

  return(parse_integer(x))
}

#' Convert string to decimal number
#'
#' M will be converted to -1.0
#' * (lost) will be converted to -2.0
#'
#' @param x string
#' @return decimal number
#' @export
#' @import tidyverse
parse_dec <- function(x) {
  x = if_else(x == "M", "-1.0",
              if_else(x == "*", "-2.0",
                      if_else(x == "N", "-3.0", x)))

  return(parse_double(x))
}

#' Convert string in d/m/Y format to dates
#'
#' Check parse_date for details
#'
#' @param dates d/m/Y string
#' @return date or NA if date is invalid
#' @export
#' @import tidyverse
parse_dmY <- function(dates) {
  return(parse_date(dates, format = "%d/%m/%Y"))
}

#' Convert id to site location
#'
#' Convert studyid to site location
#'
#' @param id studyid
#' @return string of site location or NA
#' @export
#' @import tidyverse
parse_site <- function(id) {
  site <- id %/% 10000
  case_when(
    site == 001 ~ "001-Beijing Chaoyang Hospital",
    site == 002 ~ "002-Langfang People’s Hospital",
    site == 005 ~ "005-Langfang Aidebao General Hospital",
    site == 007 ~ "007-Second Bethune Hospital of Jilin University",
    site == 008 ~ "008-Tianjin Hospital",
    site == 009 ~ "009-Beijing Anzhen Hospital",
    site == 010 ~ "010-Harbin Medical University Second Hospital",
    site == 011 ~ "011-Shenyang Orthopaedic Hospital",
    site == 013 ~ "013-Hanzhong People's Hospital",
    site == 014 ~ "014-Shanghai No.10 People's Hospital",
    site == 015 ~ "015-Xiamen University affiliated First Hospital",
    site == 016 ~ "016-The 2nd affiliated hospital of Wenzhou Medical University",
    site == 101 ~ "101-Mulago Hospital, Uganda",
    site == 102 ~ "102-Rift Valley Provincial General Hospital, Kenya",
    site == 103 ~ "103-Kenyatta National Hospital, Kenya",
    site == 104 ~ "104-REMOVED Muhimbili Orthopaedic Institute",
    site == 106 ~ "106-Kiambu District Hospital, Kenya",
    site == 108 ~ "108-KCMC - Kilimanjaro Christian Medical Centre, Tanzania",
    site == 109 ~ "109-Chris Hani Baragwanath Hospital, South Africa",
    site == 110 ~ "110-Charlotte Maxeke Johannesburg Academic Hospital, South Africa",
    site == 111 ~ "111-Helen Joseph Hospital, South Africa",
    site == 112 ~ "112-AIC Kijabe Hospital, Kenya",
    site == 113 ~ "113-REMOVED Ondo State Trauma and Surgical Centre",
    site == 114 ~ "114-Princess Marina Hospital, Botswana",
    site == 115 ~ "115-Black Lion Hospital, Addis Ababa, Ethiopia",
    site == 117 ~ "117-KATH, Kumasi, Ghana",
    site == 120 ~ "120-National Orthopedic Hospital, Enugu, Nigeria",
    site == 122 ~ "122-Baptist Hospital Mutengene, Cameroon",
    site == 201 ~ "201-Sancheti Institute of Orthopaedics",
    site == 204 ~ "204-Noble Hospital",
    site == 205 ~ "205-Bharati Vidyapeeth University Medical College",
    site == 206 ~ "206-Datta Meghe Institute of Medical Sciences",
    site == 208 ~ "208-AIIMS",
    site == 209 ~ "209-CMC Vellore",
    site == 210 ~ "210-CMC Ludhiana",
    site == 211 ~ "211-Indian Institute for Spinal Care",
    site == 212 ~ "212-IGMC & RI",
    site == 213 ~ "213-St. John's Medical Colle",
    site == 214 ~ "214-Post Graduate Institute of Medical Education and Research",
    site == 215 ~ "215-Baptist Christian Hospital",
    site == 216 ~ "216-NHL Medical College, Ahmedabad",
    site == 301 ~ "301-Northwest General Hospital & Research, Pakistan",
    site == 302 ~ "302-Lumbini Medical College, Nepal",
    site == 305 ~ "305-Cho Ray Hospital, Vietnam",
    site == 306 ~ "306-Viet Duc Hospital, Vietnam",
    site == 307 ~ "307-Ramathibodi Hospital, Thailand",
    site == 309 ~ "309-Khon Kaen Hospital, Thailand",
    site == 310 ~ "310-Philippine General Hospital, Manila, Philippines",
    site == 311 ~ "311-Sina Trauma and Surgery Research Center",
    site == 401 ~ "401-Hospital Civil de Guadalajara, Mexico",
    site == 402 ~ "402-Hospital Universitario de Caracas, Venezuela",
    site == 404 ~ "404-Hospital Sirio Libanes, Buenos Aires, Argentina",
    site == 405 ~ "405-Hospital Puerto de Hierro, Zapopan, Mexico",
    site == 407 ~ "407-Clinica Zabala, Buenos Aires, Argentina",
    site == 408 ~ "408-Ruth Paz Foundation, Honduras",
    site == 409 ~ "409-Centro Medio Imbanaco, Colombia",
    site == 410 ~ "410-Clinica Fracturas Y Fracturas, Columbia",
  )
}

#' Convert id to site region
#'
#' Convert studyid to site region
#'
#' @param id studyid
#' @return string of site region or NA
#' @export
#' @import tidyverse
parse_region <- function(id) {
  site <- id %/% 10000
  case_when(
    site < 100 ~ "China",
    site < 200 ~ "Africa",
    site < 300 ~ "India",
    site < 400 ~ "Other Asia",
    site < 500 ~ "Latin America",
  )
}

#' Merge a list of forms
#'
#' Merge a list of parsed forms by region, sit, studyid, and ptintit
#'
#' @param forms list of parsed (not raw) forms
#' @return merge dataframe
#' @export
#' @import tidyverse
merge_forms <- function(forms) {
  for (i in 1:length(forms)) {
    if (i == 1) {
      res <- forms[[i]]
    } else {
      res <- left_join(res, forms[[i]],
                       by = c("region", "site", "studyid", "ptinit"))
    }
  }

  return(res)
}

#' Convert decimal to percentage
#'
#' Convert a decimal number to a percentage as string
#' with 2 decimal places
#' 0.3 -> 30%
#'
#' @param x decimal number
#' @return string
#' @export
#' @import tidyverse
percent <- function(x) {
  round(x * 100, digits = 2)
}

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
#' 
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
#'
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
#' 
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
#'
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
#' 
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
#' 
check_form_box_III <- function(form, groups, types) {
  problems <- filter(form,(count_all(form, groups, gt_zero) > 1) | 
                       check_invalid_III(form, groups, types))
  return (problems)
}