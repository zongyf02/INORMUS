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

#' Filters out invalid rows for box 5 form 2.1
#' 
#' @param form form2.1 
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_from2.1_box5 <- function(form) {
  return (check_form_box_I(form, 13, "occup", "othoccup", "p2q5"))
}

#' Filters out invalid rows for box 10 form 2.2
#' 
#' @param form form2.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form2.2_box10 <- function(form) {
  return (check_form_box_II(form, 1, "othercm", "comorb", 
                            c("p3q101", "p3q102","p3q103", "p3q104"),
                            "nonecm", c("ischhrt","cvascd", "lowresp", 
                                        "cancer", "diabetes", "copd",
                                        "htn", "hivaids", "gidisord", 
                                        "anembld", "tb", "pneum_2.2",
                                        "malaria", "asthma", "osteo")))
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

#' Filters out invalid rows for box 5 form 3.2
#' 
#' @param form form3.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form3.2_box5 <- function(form) {
  return(case_when(is.character(form)))
}

#' Filters out invalid rows for box 6 form 3.2
#' 
#' @param form form3.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form3.2_box6 <- function(form) {
  problems <- filter(form, (transfus <= 0) |
                       (transfus == 1 & 
                          (check_invalid_I(form, 3, "transnot", 
                                           "transoth", "p5q6"))))
  return (problems)
}

#' Filters out invalid rows for box 7 form 3.2
#' 
#' @param form form3.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
#' 
check_form3.2_box7 <- function(form) {
  chest_options <- c("pneumot", "rib", "hemopneu", "hvasc", "contbr", "othchest")
  abdo_options <- c("spleen", "liver", "lbowel", "sbowel", "urethra", "bladder", 
                    "kidney", "othabdo")
  hdneck_options <- c("majfacl", "minfacl", "faclfrac", "concuss", "icbleed", "minhead",
                      "sklfrac", "othhn")
  
  problems <- filter(form, ((nonorth == 0) | (nonorth == 1 &
                                                ((chest == 1 & (check_invalid_I(form, 1, "othchest", "chestsp", c("q7chest1", "q7chest2")))) |
                                                   (chest == 0 & count_all(form, chest_options, gt_zero) > 0)) |
                                                ((abdo == 1 & (check_invalid_I(form, 1, "othabdo", "abdosp", c("q7abdo1", "q7abdo2")))) |
                                                   (abdo == 0 & count_all(form, abdo_options, gt_zero) > 0)) |
                                                (hdneck == 1 & (check_invalid_I(form, 1, "othhn", "hdnecksp", c("q7head1", "q7head2"))) |
                                                   (hdneck == 0 & count_all(form, hdneck_options, gt_zero) > 0)) |
                                                (burn == 1 & burnsev == 4 & sarea == 0) | 
                                                (burn == 0 & (burnsev != 0 | sarea != 0)))))
  return (problems)
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

#' Check that consent date should be on the same day, or after the date of injury
#' 
#' condate after injdate 0-92 days
#' 
#' @param merged_form a dataframe containing form1.1 and form3.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_condate_injdate <- function(merged_form) {
  problems <- select(merged_form,
                     region, site, studyid, ptinit, condate, injdate) %>%
    mutate(
      date_diff = difftime(parse_dmY(condate),
                           parse_dmY(injdate),
                           units = "days"),
      comment = "Consent date should be on the same day, or after the date of injury") %>%
    filter(condate != "N" & (is.na(date_diff) | date_diff < 0 | date_diff > 92))
  
  return(problems)
}

#' Check that hospital admission date should be on the same day,
#' or after the date of injury
#' 
#' hpsdate after injdate 0-92 days
#' 
#' @param merged_form a dataframe containing form1.1, form3.1, and form4.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_hspdate_injdate <- function(merged_form) {
  problems <- select(merged_form,
                     region, site, studyid, ptinit, ptstatus, hspdate, injdate) %>%
    mutate(
      diff_date = difftime(parse_dmY(hspdate),
                           parse_dmY(injdate),
                           units = "days"),
      comment = "Hospital admission date should be on the same day, or after the date of injury") %>%
    filter(ptstatus == 1 & (is.na(diff_date) | diff_date < 0 | diff_date > 92))
  
  return(problems)
}

#' Check that consent date should be 0 - 10 days after hospital admission date
#' 
#' condate after hspdate 0-10 days
#' 
#' @param merged_form a dataframe containing form1.1 and form4.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_condate_hspdate <- function(merged_form) {
  problems <- select(merged_form,
                     region, site, studyid, ptinit, condate, hspdate) %>%
    mutate(
      diff_date = difftime(parse_dmY(condate),
                           parse_dmY(hspdate), 
                           units = "days"),
      comment = "Consent date should be 0 - 10 days after hospital admission date") %>%
    filter(condate != "N" & (is.na(diff_date) | diff_date < 0 | diff_date > 30))
  
  return(problems)
}