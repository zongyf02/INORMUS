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
                              coding_boxes, disallow_any_na, disallow_all_n) {
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
  
  is_any_other_field_invalid <- FALSE
  is_any_other_field_na <- FALSE
  are_other_fields_all_n <- TRUE
  is_any_textfield_invalid <- FALSE
  num_used_text_fields <- 0
  for (i in 1:n) {
    other_field_vector <- pull(form, other_fields[i])
    text_field_vector <- pull(form, text_fields[i])
    
    is_any_other_field_invalid <- is_any_other_field_invalid |
      other_field_vector < -1
    
    is_any_other_field_na <- is_any_other_field_na |
      is.na(other_field_vector)
    
    are_other_fields_all_n <- are_other_fields_all_n &
      are_other_fields_all_n == -1
    
    is_text_field_na_vector <- is.na(text_field_vector)
    is_any_textfield_invalid <- is_any_textfield_invalid |
      is_invalid(text_field_vector) |
      if_else(other_field_vector == other_vals[i],
                is_text_field_na_vector,
              !is_text_field_na_vector)
    num_used_text_fields <- if_else(is_text_field_na_vector,
                                    num_used_text_fields,
                                    num_used_text_fields + 1)
  }

  
  return(
    is_invalid_na_or_n(ptstatus_vector) |
      (ptstatus_vector == 1 &
         (is_any_other_field_invalid |
            (disallow_any_na & is_any_other_field_na) |
            (disallow_all_n & are_other_fields_all_n) |
            is_any_textfield_invalid |
            if_else(num_used_text_fields == 0,
                    num_used_coding_boxes != 0,
                    num_used_coding_boxes < num_used_text_fields))))
}

#' Helper to filter out invalid coding boxes
#'
#' @param form any form containing ptstatus, other_fields, text_fields, and coding_boxes
#' @param other_vals int value corresponding to the 'other' option
#' @param other_fields string name of the 'other_fields' column
#' @param text_fields string name of the 'text_fields' column
#' @param coding_boxes a vector of column names for coding boxes
#' @param comment comment about specific check
#' @param disallow_any_na whether to allow NA values in other_fields
#' @param disallow_all_n whether to allow all other_fields to be N
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
check_invalid_boxes <- function(form, other_vals, other_fields, text_fields,
                                coding_boxes, comment,
                                disallow_any_na = TRUE, disallow_all_n = TRUE) {
  
  filter_cond_vector <- are_boxes_invalid(form, other_vals, other_fields,
                                          text_fields, coding_boxes,
                                          disallow_any_na, disallow_all_n)
  
  return(
    form %>%
      select(c(region, site, studyid, ptinit, ptstatus,
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
                        "Textfield and coding box 5 of form3.2 is completed if and only if the other option is chosen"))
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
                                  "Textfield and coding box 6 of form3.2 is completed if and only if the other option is chosen",
                       disallow_all_n = FALSE))
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
                        "Chest textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen",
                        disallow_all_n = FALSE))
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
                        "Abdomen textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen",
                        disallow_all_n = FALSE))
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
                        "Head/neck textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen",
                        disallow_all_n = FALSE))
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
                        "Textfield and coding box 2 of form4.1 is completed if and only if the other option is chosen"))
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
                        "Textfield and coding box 3 of form4.1 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 5 of form 4.1
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
                        "Textfield and coding box 5 of form4.1 is completed if and only if the other option is chosen",
                        disallow_all_n = FALSE))
}

#' Filters out invalid rows for box 1 of form 5.1
#' 
#' @param form a form containing ptstatus and form5.1
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.1x_box1 <- function(form, rep) {
  is_any_field_invalid <- FALSE
  num_used_text_fields <- 0
  
  oth_upper <- str_c(c("lothup", "rothup", "othupsp"), rep, sep="_")
  oth_spine <- str_c(c("lothspin", "rothspin", "othspsp"), rep, sep="_")
  oth_lower <- str_c(c("lothlo", "rothlo", "othlosp"), rep, sep="_")
  oth_pelvis <- str_c(c("lothpelv", "rothpelv", "othplvsp"), rep, sep="_")
  colname_list <- list(oth_upper, oth_spine, oth_lower, oth_pelvis)
  
  validate_fun <- if (rep == 1) {
    is_invalid_or_na
  } else {
    is_invalid
  }
  
  for (cols in colname_list) {
    vectors_list <- list()
    for (i in 1:3) {
      vectors_list[[i]] <- pull(form, cols[i])
    }
    is_textfield_na_vector <- is.na(vectors_list[[3]])
    is_any_field_invalid <- is_any_field_invalid |
      validate_fun(vectors_list[[1]]) |
      validate_fun(vectors_list[[2]]) |
      is_invalid(vectors_list[[3]]) |
      if_else((vectors_list[[1]] == 1 | vectors_list[[2]] == 1),
                is_textfield_na_vector,
              !is_textfield_na_vector)
    
    num_used_text_fields <- if_else(is_textfield_na_vector,
                                    num_used_text_fields,
                                    num_used_text_fields + 1)
  }
  
  coding_box <- str_c("injq1", rep, sep="_")
  num_used_coding_boxes <-
    if_else(is.na(pull(form, coding_box)), 0, 1)
  
  ptstatus_vector <- pull(form, ptstatus)
  filter_cond <- is_invalid_na_or_n(ptstatus_vector) |
    (ptstatus_vector == 1 &
       (is_any_field_invalid |
          if_else(num_used_text_fields == 0,
                  num_used_coding_boxes != 0,
                  num_used_coding_boxes < num_used_text_fields)))
  
  return(
    select(form, c(region, site, studyid, ptinit, ptstatus,
                   oth_upper, oth_spine, oth_lower, oth_pelvis, coding_box)) %>%
      filter(filter_cond) %>%
      mutate(comment = "Textfield and coding box 1 of form5.1/5/9 is completed if and only if the other option is chosen")
      
  )
}


#' Filters out invalid rows for box 2 of form5.2/6/10
#' 
#' @param form a form containing ptstatus and form5.2/6/10
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.2x_box2 <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("disloc", "othdloc", "injq2"), rep, sep="_")
  problems <- check_invalid_boxes(form, 12, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 2 of form5.2/6/10 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 3 "other locations of treatment" of form5.2/6/10
#' 
#' @param form a form containing ptstatus and form5.2/6/10
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.2x_box3a <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("locoth", "othloctx", "injq31", "injq312"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:4], 
                                  "Textfield and coding box 3 \"other locations of treatment\" of form5.2/6/10 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return(problems)
}

#' Filters out invalid rows for box 3 "other treatment" of form5.2/6/10
#' 
#' @param form a form containing ptstatus and form5.2/6/10
#' @param rep the repetition/iteration of the current form 
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.2x_box3b <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("othtx", "othtxsp", "injq321", "injq322", "injq323"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:5], 
                                  "Textfield and coding box 3 \"other treatment\" of form5.2/6/10 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 4 of form5.2/6/10
#' 
#' @param form a form containing ptstatus and form5.2/6/10
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.2x_box4 <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("howstab", "othstab", "injq41", "injq42", "injq43"), rep, sep="_")
  problems <- check_invalid_boxes(form, 6, group1[1], group1[2], group1[3:5], 
                                  "Textfield and coding box 4 of form5.2/6/10 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 5 "other internal fixation" of form5.3/7/11
#' 
#' @param form a form containing ptstatus and form5.3/7/11
#' @param rep the repetition/iteration of the current form 
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.3x_box5a <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("oifix", "oifixsp", "injq51o", "injq52o"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:4], 
                                  "Textfield and coding box 5 \"other internal fixation\" of form5.3/7/11 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 5 "other non-operative treatment" of form5.3/7/11
#' 
#' @param form a form containing ptstatus and form5.3/7/11
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.3x_box5b <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("othnop", "othnopsp", "injq51no", "injq52no"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:4], 
                                  "Textfield and coding box 5 \"other non-operative treatment\" of form5.3/7/11 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 7 "Other solution additive" of form5.4/8/12
#' 
#' @param form a form containing ptstatus and form5.4/8/12
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.4x_box7a <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1<- str_c(c("add", "othaddsp", "injq7a"), rep, sep="_")
  problems <- check_invalid_boxes(form, 6, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 7 \"other solution additive\" of form5.4/8/12 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 7 "Other delivery method" of form5.4/8/12
#' 
#' @param form a form containing ptstatus and form5.4/8/12
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.4x_box7b <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("delivery", "othdelsp", "injq7b"), rep, sep="_")
  problems <- check_invalid_boxes(form, 6, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 7 \"Other delivery method\" of form5.4/8/12 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 8 of form5.4/8/12
#' 
#' @param form a form containing ptstatus and form5.4/8/12
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.4x_box8 <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("failreas", "othflsp", "injq8"), rep, sep="_")
  problems <- check_invalid_boxes(form, 9, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 8 of form5.4/8/12 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 9 of form5.4/8/12
#' 
#' @param form a form containing ptstatus and form5.4/8/12 
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.4x_box9 <- function(form, rep) {
  if (rep == 1) {
    disallow_any_na <- TRUE
  } else {
    disallow_any_na <- FALSE
  }
  
  group1 <- str_c(c("delreas", "othdlysp", "injq9"), rep, sep="_")
  problems <- check_invalid_boxes(form, 9, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 9 of form5.4/8/12 is completed if and only if the other option is chosen",
                                  disallow_any_na, disallow_all_n = FALSE)
  return (problems)
}

#' Filters out invalid rows for box 1 "Other patient discharge" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box1a <- function(form) {
  return (check_invalid_boxes(form, 6, "dcto", "othdcto", "p19q1dc",
                              "Textfield and coding box 1 \"Other patient discharge\" of form6.1 is completed if and only if the other option is chosen",
                              disallow_all_n = FALSE))
}

#' Filters out invalid rows for box 1 "Cause of death" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box1b<- function(form) {
  return (check_invalid_boxes(form, 1, "deceased", "cause", c("p19q1dth", "p19q1dt2"), 
                              "Textfield and coding box 1 \"Cause of death\" of form6.1 is completed if and only if the other option is chosen",
                              disallow_all_n = FALSE))
}

#' Filters out invalid rows for box 2 "Other complication" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box2a <- function(form) {
  return (check_invalid_boxes(form, 1,"othnoc", "othnocsp", c("p19q2a","p19q2a2"), 
                              "Textfield and coding box 2 \"Other complication\" of form6.1 is completed if and only if the other option is chosen",
                              disallow_all_n = FALSE))
}

#' Filters out invalid rows for box 2 "Other cardiac complication" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box2b <- function(form) {
  return (check_invalid_boxes(form, 1, "ccoth", "ccothsp", "p19q2b", 
                              "Textfield and coding box 2 \"Other cardiac complication\" of form6.1 is completed if and only if the other option is chosen",
                              disallow_all_n = FALSE))
}


#' Filters out invalid rows for box 4 of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box4 <- function(form) {
  return (check_invalid_boxes(form, 5,"howcmpl", "othcmpl", "p19q4",
                              "Textfield and coding box 4 of form6.1 is completed if and only if the other option is chosen",
                              disallow_all_n = FALSE))
}

#' Filters out invalid rows for box 3 of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box3 <- function(form, rep) {
  group1 <- str_c(c("typecmpl", "othcmpl", "cmpq3"),rep ,sep="~")
  return (check_invalid_boxes(form, 14, group1[1], group1[2], group1[3],
                              "Textfield and coding box 3 of form7.x is completed if and only if the other option is chosen",
          disallow_any_na = FALSE))
}

#' Filters out invalid rows for box 4 "Other treatment" of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box4a <- function(form, rep) {
  group1 <- str_c(c("mancomp", "manoth", "cmpq4a"),rep ,sep="~")
  return (check_invalid_boxes(form, 14, group1[1], group1[2], group1[3],
                              "Textfield and coding box 4 \"Other treatment\" of form7.x is completed if and only if the other option is chosen",
                              disallow_any_na = FALSE))
}

#' Filters out invalid rows for box 4 "Other non-operative treatment" of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box4b <- function(form, rep) {
  group1 <- str_c(c("nother", "nothersp", "cmpq4b"),rep ,sep="~")
  return (check_invalid_boxes(form, 14, group1[1], group1[2], group1[3],
                              "Textfield and coding box 4 \"Other non-operative treatment\" of form7.x is completed if and only if the other option is chosen",
          disallow_any_na = FALSE))
}


#' Filters out invalid rows for box 4 "Other details" of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box4c <- function(form, rep) {
  group1 <- str_c(c("mother", "mothersp", "cmpq4c"),rep ,sep="~")
  return (check_invalid_boxes(form, 14, group1[1], group1[2], group1[3],
                              "Textfield and coding box 4 \"Other details\" of form7.x is completed if and only if the other option is chosen",
          disallow_any_na = FALSE))
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
      region, site, studyid, ptinit, ptstatus, condate, injdate,
      date_diff = difftime(parse_dmY(condate),
                           parse_dmY(injdate),
                           units = "days"),
      comment = "Consent date should be on the same day, or after the date of injury") %>%
    filter(is_invalid_na_or_n(ptstatus) |
             (ptstatus == 1 &
                (is.na(date_diff) | date_diff < 0 | date_diff > 92)))
  
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
    filter(is_invalid_na_or_n(ptstatus) |
             (ptstatus == 1 &
                (is.na(diff_date) | diff_date < 0 | diff_date > 92)))
  
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
      region, site, studyid, ptinit, ptstatus, condate, hspdate,
      diff_date = difftime(parse_dmY(condate),
                           parse_dmY(hspdate), 
                           units = "days"),
      comment = "Consent date should be 0 - 10 days after hospital admission date") %>%
    filter(is_invalid_na_or_n(ptstatus) |
             (ptstatus == 1
              & (is.na(diff_date) | diff_date < 0 | diff_date > 30)))
  
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
      is_invalid_na_or_n(ptstatus) |
        (ptstatus == 1 &
           (is.na(date_diff) | is_invalid_na_or_n(ihunits)  | ihunits == 0 | 
              (ihunits == 1 &
                 (is_invalid_na_or_n(ihhrs) | (ihhrs < (date_diff - 24)) | (ihhrs > (date_diff + 24)))) |
              (ihunits == 2 &
                 (is_invalid_na_or_n(ihdays) | (ihdays * 24 < (date_diff - 24)) | (ihdays * 24 > (date_diff + 24)))))))
  
  return(problems)
}

#' Check that the number of ortho injuries stated on form 3.2 is consistent 
#' with the number of sets of injury forms completed
#' 
#' @param form dataframe containing pststatus and form 3.2 
#' @return a dataframe containing problematic entries
#' @import tidyverse
#' @export 
check_northinj <- function(form) {
  col_names <- colnames(form)
  set1 <- str_which(col_names, "_1$")
  set2 <- str_which(col_names, "_2$")
  set3 <- str_which(col_names, "_3$")
  
  is_set1_empty <- TRUE
  for(i in set1) {
    is_set1_empty <- is_set1_empty & is.na(pull(form, i))
  }
  
  is_set2_empty <- TRUE
  for(i in set2) {
    is_set2_empty <- is_set2_empty & is.na(pull(form, i))
  }
  
  is_set3_empty <- TRUE
  for(i in set3) {
    is_set3_empty <- is_set3_empty & is.na(pull(form, i))
  }
  
  
  problems <- form %>% 
    transmute(
      region, site, studyid, ptinit, ptstatus, northinj,
      is_set1_empty = is_set1_empty, continue_1,
      is_set2_empty  = is_set2_empty, continue_2, 
      is_set3_empty  = is_set3_empty,
      comment = "The number of ortho injuries stated on form 3.2 should be consistent with the number of sets of injury forms completed") %>% 
    filter(
      is_invalid_na_or_n(ptstatus) |
        (ptstatus == 1 &
           (is_invalid_na_or_n(northinj) |
              ((northinj != 0 | !is_set1_empty | !is_set2_empty | !is_set3_empty) &
                 (northinj != 1 | is_set1_empty | !is_set2_empty | !is_set3_empty
                  | is.na(continue_1) | continue_1 != 0) &
                 (northinj != 2 | is_set1_empty | is_set2_empty |
                    !is_set3_empty | is.na(continue_1) | is.na(continue_2) |
                    continue_1 != 1 | continue_2 != 0) &
                 (northinj != 3 | is_set1_empty | is_set2_empty |
                    is_set3_empty | is.na(continue_1) | is.na(continue_2) |
                    continue_1 != 1 | continue_2 != 1)))))
  return (problems)
}

#' The time from injury to hospital admission should be within 24 hours
#' if the patient is coming from the Accident/Injury Site 
#' 
#' @param merged_form a dataframe containing ptstatus and form4.1
#' @return a dataframe containing problematic entries with relevant columns
#' 
#' @import tidyverse
#' @export
check_admfrom_ihunits <- function(forms) {
  problems <- transmute(forms,
                        region, site, studyid, ptinit, ptstatus, admfrom, ihunits, ihhrs, ihdays,
                        comment = "The time from injury to hospital admission should be within 24 hours if the patient is coming from the Accident/Injury Site") %>%
    filter(is_invalid_na_or_n(ptstatus) |
             (ptstatus == 1 &
                (is_invalid_na_or_n(admfrom) | is_invalid_na_or_n(ihunits)|
                   is_invalid_or_n(ihhrs) | is_invalid_or_n(ihdays) |
                   (admfrom  == 1 & (ihunits != 1 | (ihunits == 1 & ihhrs > 24))))))
  
  
  return(problems)
  
}