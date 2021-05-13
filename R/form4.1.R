#' Read in form4.1
#'
#' Read in form4.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form4.1 dataframe
#' @export
#' @import tidyverse
read_form4.1 <- function(path, raw = FALSE){
  form <- read_delim(path, delim = "|",
                     col_types = "------iccccccccccccccccccccccccc----")

  if(!raw){
    form <- form %>%
      mutate(across(c(4, 6, 8:11, 13:26), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid))%>%
      relocate(c(region, site), .before = studyid)

  }
  return(form)
}

#' Filters out invalid rows for box 2 form 4.1
#' 
#' @param form form4.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form4.1_box2 <- function(form) {
  problems <- check_form_box_I(form, 6, "admfrom", "othfrom", "q6p2")
  return (problems)
}

#' Filters out invalid rows for box 3 form 4.1
#' 
#' @param form form4.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form4.1_box3 <- function(form) {
  problems <- check_form_box_I(form, 8,"transto", "othto", "p6q3")
  return (problems)
}

#' Filters out invalid rows for box 5 form 4.1
#' 
#' @param form form4.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form4.1_box5 <- function(form) {
  return (check_form_box_I(form, 10, "rsdelay", "othdelay", "p6q5"))
}

#' Filters out invalid rows for box 6 form 4.1
#' 
#' @param form form4.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form4.1_box6 <- function(form) {
  group1 <- c("injscene", "erinhosp", "preop", "oper","postop", "dnradabx")
  group2 <- c("injscene", "erinhosp", "preop", "oper","postop")
  
  problems <- filter(form, (abx == 0) | ((abx == 1) & 
                                           (((iaunits == 1) & ((iahrs < 0) | (iahrs > 72))) |
                                              ((iaunits == 2) & (iadays <= 3)) | (iaunits == 0)) |
                                           (locabx == 0) |
                                           (is_all(form, group1, is_zero)) | 
                                           (dnradabx == 1 & (count_all(form, group2, gt_zero) > 0))))
  return (problems)
}