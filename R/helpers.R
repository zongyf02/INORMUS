#' Convert string in d/m/Y format to dates
#'
#' Check parse_date for details
#'
#' @param dates d/m/Y string
#' @return date or NA if date is invalid
#' @export
#' @import tidyverse
parse_dmY <- function(dates) {
  return(parse_date(as.character(dates), format = "%d/%m/%Y"))
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
      res <- full_join(res, forms[[i]],
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
  str_c(round(x * 100, digits = 2), "%")
}


#' Check if numeric or character vector is invalid
#'
#' invalid means 'M', or '*' for character or
#' <= -2 for numeric
#'
#' @param x number or character
#' @return TRUE/FALSE
#' @export
#' @import tidyverse
is_invalid <- function(x) {
  if (is.character(x)) {
    x == "M" | x == "*"
  } else if (is.numeric(x)) {
    x < -1
  } else {
    stop("Argument not a character or number", x)
  }
}

#' Check if numeric or character vector is N
#'
#' N means 'N' for character or
#' or -1 for numeric
#'
#' @param x number or character
#' @return TRUE/FALSE
#' @export
#' @import tidyverse
is_n <- function(x) {
  if (is.character(x)) {
    x == "N"
  } else if (is.numeric(x)) {
    x == -1
  } else {
    stop("Argument not a character or number", x)
  }
}

#' Check if numeric or character vector is invalid or NA
#'
#' Check is_invalid for definition of invalid
#'
#' @param x number or character
#' @return TRUE/FALSE
#' @export
#' @import tidyverse
is_invalid_or_na <- function(x) {
  is.na(x) | is_invalid(x)
}

#' Check if numeric or character vector is invalid or N
#' 
#' Check is_invalid and is_n for definitions of invalid and N
#'
#' @param x number or character
#' @return TRUE/FALSE
#' @export
#' @import tidyverse
is_invalid_or_n <- function(x) {
  if (is.character(x)) {
    x == "M" | x == "*" | x == "N"
  } else if (is.numeric(x)) {
    x < 0
  } else {
    stop("Argument not a character or number", x)
  }
}

#' Check if numeric or character vector is invalid, NA or N
#' 
#' Check is_invalid and is_n for definitions of invalid and N
#'
#' @param x number or character
#' @return TRUE/FALSE
#' @export
#' @import tidyverse
is_invalid_na_or_n <- function(x) {
  is.na(x) | is_invalid_or_n(x)
}

#' Filter out patients who were admitted to a hospital pre-covid
#' Defined as before March 11, 2020
#' 
#' @param form form containing hspdate
#' @return filtered tibble
#' 
#' @export
#' @import tidyverse
filter_precovid <- function(form) {
  form %>% filter(
    parse_dmY(hspdate) < parse_dmY("11/03/2020")
  )
}
