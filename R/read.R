#' Convert integer to logicals
#'
#' 0: NA
#' 1: TRUE
#' 2: FALSE
#'
#' @param x integer
#' @return NA/T/F
#' @export
#' @import tidyverse
parse_tf <- function(x) {
  case_when(
    x == 0 ~ NA,
    x == 1 ~ TRUE,
    x == 2 ~ FALSE)
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
    site == 001 ~ "Beijing Chaoyang Hospital",
    site == 002 ~ "Langfang People’s Hospital",
    site == 005 ~ "Langfang Aidebao General Hospital",
    site == 007 ~ "Second Bethune Hospital of Jilin University",
    site == 008~ "Tianjin Hospital",
    site == 009 ~ "Beijing Anzhen Hospital",
    site == 010 ~ "Harbin Medical University Second Hospital",
    site == 011 ~ "Shenyang Orthopaedic Hospital",
    site == 013 ~ "Hanzhong People's Hospital",
    site == 014 ~ "Shanghai No.10 People's Hospital",
    site == 015 ~ "Xiamen University affiliated First Hospital",
    site == 016 ~ "The 2nd affiliated hospital of Wenzhou Medical University",
    site == 101 ~ "Mulago Hospital, Uganda",
    site == 102 ~ "Rift Valley Provincial General Hospital, Kenya",
    site == 103 ~ "Kenyatta National Hospital, Kenya",
    site == 104 ~ "REMOVED Muhimbili Orthopaedic Institute",
    site == 106 ~ "Kiambu District Hospital, Kenya",
    site == 108 ~ "KCMC - Kilimanjaro Christian Medical Centre, Tanzania",
    site == 109 ~ "Chris Hani Baragwanath Hospital, South Africa",
    site == 110 ~ "Charlotte Maxeke Johannesburg Academic Hospital, South Africa",
    site == 111 ~ "Helen Joseph Hospital, South Africa",
    site == 112 ~ "AIC Kijabe Hospital, Kenya",
    site == 113 ~ "REMOVED Ondo State Trauma and Surgical Centre",
    site == 114 ~ "Princess Marina Hospital, Botswana",
    site == 115 ~ "Black Lion Hospital, Addis Ababa, Ethiopia",
    site == 117 ~ "KATH, Kumasi, Ghana",
    site == 120 ~ "National Orthopedic Hospital, Enugu, Nigeria",
    site == 122 ~ "Baptist Hospital Mutengene, Cameroon",
    site == 201 ~ "Sancheti Institute of Orthopaedics",
    site == 204 ~ "Noble Hospital",
    site == 205 ~ "Bharati Vidyapeeth University Medical College",
    site == 206 ~ "Datta Meghe Institute of Medical Sciences",
    site == 208 ~ "AIIMS",
    site == 209 ~ "CMC Vellore",
    site == 210 ~ "CMC Ludhiana",
    site == 211 ~ "Indian Institute for Spinal Care",
    site == 212 ~ "IGMC & RI",
    site == 213 ~ "St. John's Medical Colle",
    site == 214 ~ "Post Graduate Institute of Medical Education and Research",
    site == 215 ~ "Baptist Christian Hospital",
    site == 216 ~ "NHL Medical College, Ahmedabad",
    site == 301 ~ "Northwest General Hospital & Research, Pakistan",
    site == 302 ~ "Lumbini Medical College, Nepal",
    site == 305 ~ "Cho Ray Hospital, Vietnam",
    site == 306 ~ "Viet Duc Hospital, Vietnam",
    site == 307 ~ "Ramathibodi Hospital, Thailand",
    site == 309 ~ "Khon Kaen Hospital, Thailand",
    site == 310 ~ "Philippine General Hospital, Manila, Philippines",
    site == 311 ~ "Sina Trauma and Surgery Research Center",
    site == 401 ~ "Hospital Civil de Guadalajara, Mexico",
    site == 402 ~ "Hospital Universitario de Caracas, Venezuela",
    site == 404 ~ "Hospital Sirio Libanes, Buenos Aires, Argentina",
    site == 405 ~ "Hospital Puerto de Hierro, Zapopan, Mexico",
    site == 407 ~ "Clinica Zabala, Buenos Aires, Argentina",
    site == 408 ~ "Ruth Paz Foundation, Honduras",
    site == 409 ~ "Centro Medio Imbanaco, Colombia",
    site == 410 ~ "Clinica Fracturas Y Fracturas, Columbia",
  )
}

#' Convert id to site region
#'
#'Convert studyid to site region
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


#' Read in form1.1
#'
#' Read in form1.1 from path
#' Convert integers to logicals
#'
#' @param path path to file
#' @return form1.1 dataframe
#' @export
#' @import tidyverse
read_form1.1 <- function(path) {
  form <- suppressWarnings(read_delim(
    path, delim = "|",
    col_types = "------iciiiiiic----"))

  form <- form %>%
    mutate(across(3:8, parse_tf),
           condate = parse_date(condate, format = "%d/%m/%Y", na = "N"),
           region = parse_region(studyid),
           site = parse_site(studyid)) %>%
    relocate(c(region, site), .before = studyid)

  return(form)
}
