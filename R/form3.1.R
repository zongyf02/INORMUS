#' Read in form3.1
#'
#' Read in form3.1 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form3.1 dataframe
#' @export
#' @import tidyverse
read_form3.1 <- function(path, raw = FALSE) {
  form <- read_delim (path, delim = "|",
                col_types = "------icccccccccccccccccccccccccc----")

 if (!raw) {
   form <- form %>%
     mutate(across(c(4:7, 9:17, 19:20, 22:23, 25:27), parse_int),
            region = parse_region(studyid),
            site = parse_site(studyid)) %>%
     relocate(c(region, site), .before = studyid)
 }

  return(form)
}

#' Filters out invalid rows for box 4 form 3.1
#' 
#' @param form form3.1 
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form3.1_box4 <- function(form) {
  group1 <- c("trans", "fall", "intent", "strklift", "othmech")
  group2 <- list(c("trans", 10, "transsp","othtrans", "p4q4"),
                 c("intent", 6, "intentsp","othinten", "p4q4"),
                 c("strklift", 4, "stliftsp", "othstlif", "p4q4"), 
                 c("othmech", 5,"omechsp", "othmoth", "p4q4"))
  types <- c("single","single", "single", "single")
  problems <- filter(form, (count_all(form, group1, is_zero) != 4) 
                     | (check_invalid_III(form, group2))
                     | (trans == 1 & 
                          ((transsp == 6 & bhelm == 0) |
                             (transsp == 7 & mhelm == 0) |
                             (transsp == 8 & tbsbelt == 0) |
                             (transsp == 9 & asbelt == 0))))
  
  
  return (problems)
}