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

#' Convert string in d/m/Y format to date
#'
#' Check parse_date for details
#'
#' @param date d/m/Y string
#' @return date
#' @export
#' @import tidyverse
parse_dmY <- function(date) {
  return(parse_date(date, format = "%d/%m/%Y",
                    na = c("", "NA", "N", "*", "M")))
}

#' Convert id to site location
#'
#'Convert studyid to site location
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
