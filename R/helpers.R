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
    site == 002 ~ "002-Langfang People's Hospital",
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
    site == 303 ~ "303-Jigme Dorji Wangchuck National Referral Hospital",
    site == 305 ~ "305-Cho Ray Hospital, Vietnam",
    site == 306 ~ "306-Viet Duc Hospital, Vietnam",
    site == 307 ~ "307-Ramathibodi Hospital, Thailand",
    site == 309 ~ "309-Khon Kaen Hospital, Thailand",
    site == 310 ~ "310-Philippine General Hospital, Manila, Philippines",
    site == 311 ~ "311-Sina Trauma and Surgery Research Center",
    site == 401 ~ "401-Hospital Civil de Guadalajara, Mexico",
    site == 402 ~ "402-Hospital Universitario de Caracas, Venezuela",
    site == 403 ~ "403-Hospital Central de IPS, Paraguay",
    site == 404 ~ "404-Hospital Sirio Libanes, Buenos Aires, Argentina",
    site == 405 ~ "405-Hospital Puerto de Hierro, Zapopan, Mexico",
    site == 406 ~ "406-Hospital de Clinicas - UNICAMP, Brazil",
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
