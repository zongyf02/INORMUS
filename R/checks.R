#' Helper to determine if coding boxes are valid
#'
#' @param form any form containing ptstatus, other_fields, text_fields, and coding_boxes
#' @param other_vals int value corresponding to the 'other' option
#' @param other_fields string name of the 'other_fields' column
#' @param text_fields string name of the 'text_fields' column
#' @param coding_boxes a vector of column names for coding boxes
#' 
#' @return a vector of logical values
#' 
#' @import tidyverse
are_boxes_invalid <- function(form, other_vals, other_fields, text_fields,
                              coding_boxes) {
  n <- length(other_vals)
  if (n != length(other_fields) || n != length(text_fields)) {
    stop("Length of other_vals, other_fields, and text_fields don't match")
  }
  
  ptstatus_vector <- pull(form, ptstatus)
  
  num_used_coding_boxes <- 0
  for (coding_box in coding_boxes) {
    num_used_coding_boxes <- if_else(is.na(pull(form, coding_box)),
                                     num_used_coding_boxes,
                                     num_used_coding_boxes + 1)
  }
  
  are_other_fields_all_invalid <- TRUE
  are_other_fields_all_na <- TRUE
  are_textfields_invalid <- FALSE
  num_used_text_fields <- 0
  for (i in 1:n) {
    other_field_vector <- pull(form, other_fields[i])
    is_text_field_na_vector <- is.na(pull(form, text_fields[i]))
    
    are_other_fields_all_invalid <- are_other_fields_all_invalid &
      other_field_vector <= -2
    
    are_other_fields_all_na <- are_other_fields_all_na &
      is.na(other_field_vector)
    
    are_textfields_invalid <- are_textfields_invalid |
      if_else(other_field_vector == other_vals[i],
              is_text_field_na_vector,
              !is_text_field_na_vector)

    num_used_text_fields <- if_else(is_text_field_na_vector,
                                    num_used_text_fields,
                                    num_used_text_fields + 1)
  }

  
  return(
    ptstatus_vector == 1 &
      (are_other_fields_all_invalid |
         are_other_fields_all_na |
         are_textfields_invalid |
         if_else(num_used_text_fields == 0,
                 num_used_coding_boxes != 0,
                 num_used_coding_boxes < num_used_text_fields)))
}

#' Helper to filter out invalid coding boxes
#'
#' @param form any form containing ptstatus, other_fields, text_fields, and coding_boxes
#' @param other_vals int value corresponding to the 'other' option
#' @param other_fields string name of the 'other_fields' column
#' @param text_fields string name of the 'text_fields' column
#' @param coding_boxes a vector of column names for coding boxes
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
check_invalid_boxes <- function(form, other_vals, other_fields, text_fields,
                                coding_boxes, comment) {
  filter_cond_vector <- are_boxes_invalid(form, other_vals, other_fields,
                                          text_fields, coding_boxes)
  
  return(
    form %>%
      select(c(region, site, studyid, ptinit,
               unlist(map2(other_fields, text_fields, c)), coding_boxes)) %>%
      filter(filter_cond_vector) %>%
      mutate(comment = comment)
  )
}

#' Filters out invalid rows for box 5 of form2.1
#' 
#' @param form a form containing ptstatus and form2.1 
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form2.1_box5 <- function(form) {
  return(check_invalid_boxes(form, 13, "occup", "othoccup", "p2q5",
                           "Textfield and coding box 5 of form2.2 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 10 of form2.2
#' 
#' @param form a form containing ptstatus and form2.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form2.2_box10 <- function(form) {
  return(check_invalid_boxes(form, 1, "othercm", "comorb",
                           c("p3q101", "p3q102","p3q103", "p3q104"),
                           "Textfield and coding box 10 of form2.2 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 4 of form3.1
#' 
#' @param form a form containing ptstatus and form3.1 
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.1_box4 <- function(form) {
  return(check_invalid_boxes(form, c(10, 6, 4, 5),
                             c("transsp", "intentsp", "stliftsp", "omechsp"),
                             c("othtrans", "othinten", "othstlif", "othmoth"),
                             "p4q4",
                             "Textfield and coding box 4 of form3.1 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 5 of form3.2
#' 
#' @param form a form containing ptstatus and form3.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.2_box5 <- function(form) {
  return(
    check_invalid_boxes(form, 9, "placeinj", "othplace", "p5q5",
                        "Textfield and coding box 5 of form3.2 is completed if and only if the other option is chosen")
  )
}

#' Filters out invalid rows for box 6 of form3.2
#' 
#' @param form a form containing ptstatus and form3.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.2_box6 <- function(form) {
  return(
   check_invalid_boxes(form, 3, "transnot", "transoth", "p5q6",
                                  "Textfield and coding box 6 of form3.2 is completed if and only if the other option is chosen")
  )
}

#' Filters out invalid rows for the chest box 7 of form3.2
#' 
#' @param form a form containing ptstatus and form3.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.2_box7_chest <- function(form) {
  return(
    check_invalid_boxes(form, 1, "othchest", "chestsp", c("q7chest1", "q7chest2"),
                        "Chest textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen")
  )
}

#' Filters out invalid rows for the abdomen box 7 of form3.2
#' 
#' @param form form3.2
#' 
#' @return a form containing ptstatus and a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.2_box7_abdo <- function(form) {
  return(
    check_invalid_boxes(form, 1, "othabdo", "abdosp", c("q7abdo1", "q7abdo2"),
                        "Abdomen textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen")
  )
}

#' Filters out invalid rows for the head/neck box 7 of form3.2
#' 
#' @param form a form containing ptstatus and form3.2
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.2_box7_hn <- function(form) {
  return(
    check_invalid_boxes(form, 1, "othhn", "hdnecksp", c("q7head1", "q7head2"),
                        "Head/neck textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen")
  )
}

#' Filters out invalid rows for box 2 of form4.1
#' 
#' @param form a form containing ptstatus and form4.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form4.1_box2 <- function(form) {
  return(
    check_invalid_boxes(form, 6, "admfrom", "othfrom", "q6p2",
                        "Textfield and coding box 2 of form4.1 is completed if and only if the other option is chosen")
  )
}

#' Filters out invalid rows for box 3 of form4.1
#' 
#' @param form a form containing ptstatus and form4.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form4.1_box3 <- function(form) {
  return(
    check_invalid_boxes(form, 8, "transto", "othto", "p6q3",
                        "Textfield and coding box 3 of form4.1 is completed if and only if the other option is chosen")
  )
}

#' Filters out invalid rows for box 5 form of 4.1
#' 
#' @param form a form containing ptstatus and form4.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form4.1_box5 <- function(form) {
  return(
    check_invalid_boxes(form, 10, "rsdelay", "othdelay", "p6q5",
                        "Textfield and coding box 5 of form4.1 is completed if and only if the other option is chosen")
    )
}

#' Check that consent date should be on the same day, or after the date of injury
#' 
#' condate after injdate 0-92 days
#' 
#' @param form a dataframe containing form1.1 and form3.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_condate_injdate <- function(form) {
  problems <- form %>%
    transmute(
      region, site, studyid, ptinit, condate, injdate,
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
#' @param form a dataframe containing form1.1, form3.1, and form4.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_hspdate_injdate <- function(form) {
  problems <- form %>%
    transmute(
      region, site, studyid, ptinit, ptstatus, hspdate, injdate,
      diff_date = difftime(parse_dmY(hspdate),
                           parse_dmY(injdate),
                           units = "days"),
      comment = "Hospital admission date should be on the same day, or after the date of injury") %>%
    filter(ptstatus == 1 & (is.na(diff_date) | diff_date < 0 | diff_date > 92))
  
  return(problems)
}

#' Check that consent date should be 0 - 30 days after hospital admission date
#' 
#' condate after hspdate 0-30 days
#' 
#' @param form a dataframe containing form1.1 and form4.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_condate_hspdate <- function(form) {
  problems <- form %>%
    transmute(
      region, site, studyid, ptinit, condate, hspdate,
      diff_date = difftime(parse_dmY(condate),
                           parse_dmY(hspdate), 
                           units = "days"),
      comment = "Consent date should be 0 - 10 days after hospital admission date") %>%
    filter(condate != "N" & (is.na(diff_date) | diff_date < 0 | diff_date > 30))
  
  return(problems)
}

#' Check that time from injury to hsp admission should be within +/- 24 hrs
#' range of difference between injdate and hspdate
#' 
#' @param form a dataframe containing form1.1, form3.1, form4.1
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export
check_injdate_hspdate <- function(form) {
  problems <- form %>%
    transmute(
      region, site, studyid, ptinit, ptstatus, injdate, hspdate, ihunits, ihhrs,
      ihdays, date_diff = difftime(parse_dmY(hspdate),
                                   parse_dmY(injdate),
                                   units = "hours"),
      comment = "Time of injury to hsp admission should be within +/- 24 hr range of date difference between injdate and hspdate") %>%
    filter(
      ptstatus == 1 &
        (is.na(date_diff) | ihunits == 0 | 
           (ihunits == 1 &
              (is.na(ihhrs) | ihhrs < 0 | (ihhrs < (date_diff - 24)) | (ihhrs > (date_diff + 24)))) |
           (ihunits == 2 &
              (is.na(ihdays) | ihdays < 0 | (ihdays * 24 < (date_diff - 24)) | (ihdays * 24 > (date_diff + 24))))))
  
  return(problems)
}

#' Check that the number of ortho injuries stated on form 3.2 is consistent 
#' with the number of sets of injury forms completed
#' 
#' @param form dataframe containing all forms 
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export 
check_northinj <- function(form) {
  not_all_na <- function(x) {
    !all(is.na(x))
  }
  
  problems <- form %>% 
    transmute(
      region, site, studyid, ptinit, northinj,
      is_set1_filled = apply(select(form, ends_with("_1")), 1, not_all_na),
      continue_1,
      is_set2_filled  = apply(select(form, ends_with("_2")), 1, not_all_na),
      continue_2, 
      is_set3_filled  = apply(select(form, ends_with("_3")), 1, not_all_na),
      comment = "The number of ortho injuries stated on form 3.2 should be consistent with the number of sets of injury forms completed") %>% 
    filter(
      is.na(northinj) | northinj < 0 |
        !((northinj == 1 & is_set1_filled == TRUE & is_set2_filled == FALSE
           & is_set3_filled == FALSE & continue_1 == 0) |
            (northinj == 2 & is_set1_filled == TRUE & is_set2_filled == TRUE
             & is_set3_filled == FALSE & continue_1 == 1 & continue_2 == 0) |
            (northinj == 3 & is_set1_filled == TRUE & is_set2_filled == TRUE
             & is_set3_filled == TRUE & continue_1 == 1 & continue_2 == 1)))
  return (problems)
}
