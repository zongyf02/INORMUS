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

#' Filter out patients who have a hip injury (lpfem or rpfem)
#' 
#' @param form merged fpr,
#' @return filtered tibble
#' 
#' @export
#' @import tidyverse
filter_hip <- function(form) {
  form %>%
    transmute(region, site, studyid, ptinit, ptstatus, lpfem_1, rpfem_1,
              lpfem_2, rpfem_2, lpfem_3, rpfem_3, age, sex, educ, locat,
              occup, othoccup, p2q5, income, hlthins, nonecm, ischhrt,
              cvascd, lowresp, cancer, diabetes, copd, htn, hivaids,
              gidisord, anembld, tb, pneum_2.2, malaria, asthma, osteo,
              othercm, comorb, p3q101, p3q102, p3q103, p3q104, onrxd,
              fall, fallfrom, lowhigh, placeinj, othplace, p5q5, admfrom,
              othfrom, q6p2, transto, othto, p6q3, ihunits, ihhrs,
              ihdays, rsdelay, othdelay, transnot, transoth, p5q6, abx,
              injscene, erinhosp, preop, oper, postop, dnradabx,
              abxprior_1, abxprior_2, abxprior_3, howstab_1, howstab_2,
              howstab_3, othstab_1, othstab_2, othstab_3, injq41_1,
              injq41_2, injq41_3, injq42_1, injq42_2, injq42_3, injq43_1,
              injq43_2, injq43_3, operat_1, operat_2, operat_3,
              hsunits_1, hsunits_2, hsunits_3, ishrs_1, ishrs_2, ishrs_3,
              isdays_1, isdays_2, isdays_3, intfix_1, intfix_2, intfix_3,
              screws_1, screws_2, screws_3, kwire_1, kwire_2, kwire_3,
              ampu_1, ampu_2, ampu_3, arthro_1, arthro_2, arthro_3,
              jfusion_1,  jfusion_2,  jfusion_3, imnail_1, imnail_2,
              imnail_3, reamed_1, reamed_2, reamed_3, sign_1, sign_2,
              sign_3, unreamed_1, unreamed_2, unreamed_3, pands_1,
              pands_2,  pands_3, locked_1, locked_2, locked_3, nlocked_1,
              nlocked_2, nlocked_3, sincsub_1, sincsub_2, sincsub_3,
              tdissect_1, tdissect_2, tdissect_3, wire_1, wire_2, wire_3,
              synth_1, synth_2, synth_3, cable_1, cable_2, cable_3, oifix_1,
              oifix_2, oifix_3, oifixsp_1, oifixsp_2, oifixsp_3, extfix_1,
              extfix_2, extfix_3, noperat_1, noperat_2, noperat_3,
              plaster_1, plaster_2, plaster_3, traction_1, traction_2,
              traction_3, othnop_1,  othnop_2,  othnop_3, othnopsp_1,
              othnopsp_2, othnopsp_3, injq51no_1, injq51no_2, injq51no_3,
              injq52no_1, injq52no_2, injq52no_3, injq51o_1, injq51o_2,
              injq51o_3, injq52o_1, injq52o_2, injq52o_3, failsurg_1,
              failsurg_2, failsurg_3, failreas_1, failreas_2, failreas_3,
              othflsp_1, othflsp_2, othflsp_3, injq8_1, injq8_2, injq8_3,
              delsurg_1, delsurg_2, delsurg_3, delreas_1, delreas_2,
              delreas_3, othdlysp_1, othdlysp_2, othdlysp_3, injq9_1,
              injq9_2, injq9_3, hspdate, stillhsp, dchosp, hdcdate) %>%
    filter(lpfem_1 == 1 | rpfem_1 == 1 | lpfem_2 == 1 | rpfem_2 == 1 |
             lpfem_3 == 1 |rpfem_3 == 1)
}