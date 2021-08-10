#' Helper to filter out invalid coding boxes
#'
#' @param form any form containing ptstatus, other_fields, text_fields, and coding_boxes
#' @param other_vals int value corresponding to the 'other' option
#' @param other_fields string name of the 'other_fields' column
#' @param text_fields string name of the 'text_fields' column
#' @param coding_boxes a vector of column names for coding boxes
#' @param comment comment about specific check
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
check_invalid_boxes <- function(form, other_vals, other_fields, text_fields,
                                coding_boxes, comment) {
  
  n <- length(other_vals)
  if (n != length(other_fields) || n != length(text_fields)) {
    stop("Length of other_vals, other_fields, and text_fields don't match")
  }
  
  num_used_coding_boxes <- 0
  for (coding_box in coding_boxes) {
    num_used_coding_boxes <- if_else(is.na(pull(form, coding_box)),
                                     num_used_coding_boxes,
                                     num_used_coding_boxes + 1)
  }
  
  is_any_textfield_invalid <- FALSE
  num_used_text_fields <- 0
  for (i in 1:n) {
    is_text_field_na_vector <- is.na(pull(form, text_fields[i]))
    is_any_textfield_invalid <- is_any_textfield_invalid |
      if_else(pull(form, other_fields[i]) == other_vals[i],
              is_text_field_na_vector,
              !is_text_field_na_vector)
    
    num_used_text_fields <- if_else(is_text_field_na_vector,
                                    num_used_text_fields,
                                    num_used_text_fields + 1)
  }
  
  return(
    form %>%
      select(c(region, site, studyid, ptinit, ptstatus,
               unlist(map2(other_fields, text_fields, c)), coding_boxes)) %>%
      filter(ptstatus == 1 &
               (is_any_textfield_invalid |
                  if_else(num_used_text_fields == 0,
                          num_used_coding_boxes != 0,
                          num_used_coding_boxes < num_used_text_fields))) %>%
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
                        "Textfield and coding box 6 of form3.2 is completed if and only if the other option is chosen"))
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
                        "Chest textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen"))
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
                        "Abdomen textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen"))
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
                        "Head/neck textfield and coding box 7 of form3.2 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for district code for form3.3
#' 
#' @param form a form containing ptstatus and form3.3
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.3_p50dist <- function(form) {
  form %>%
    transmute(region, site, studyid, ptinit, ptstatus, district, p50dist,
              comment = "District code needs to be completed if disctrict is entered") %>%
    filter(ptstatus == 1 & (
      !is.na(district) & is.na(p50dist)
    ))
}

#' Filters out invalid rows for city code for form3.3
#' 
#' @param form a form containing ptstatus and form3.3
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form3.3_p50city <- function(form) {
  form %>%
    transmute(region, site, studyid, ptinit, ptstatus, cityprov, p50city,
              comment = "City code needs to be completed if city is entered") %>%
    filter(ptstatus == 1 & (
      !is.na(cityprov) & is.na(p50city)
    ))
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
                        "Textfield and coding box 5 of form4.1 is completed if and only if the other option is chosen"))
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
  is_text_field_invalid <- FALSE
  num_used_text_fields <- 0
  
  oth_upper <- str_c(c("lothup", "rothup", "othupsp"), rep, sep="_")
  oth_spine <- str_c(c("lothspin", "rothspin", "othspsp"), rep, sep="_")
  oth_lower <- str_c(c("lothlo", "rothlo", "othlosp"), rep, sep="_")
  oth_pelvis <- str_c(c("lothpelv", "rothpelv", "othplvsp"), rep, sep="_")
  colname_list <- list(oth_upper, oth_spine, oth_lower, oth_pelvis)
  
  for (cols in colname_list) {
    vectors_list <- list()
    for (i in 1:3) {
      vectors_list[[i]] <- pull(form, cols[i])
    }
    is_text_field_na_vector <- is.na(vectors_list[[3]])
    is_text_field_invalid <- is_text_field_invalid |
      if_else((vectors_list[[1]] == 1 | vectors_list[[2]] == 1),
              is_text_field_na_vector,
              !is_text_field_na_vector)
    
    num_used_text_fields <- if_else(is_text_field_na_vector,
                                    num_used_text_fields,
                                    num_used_text_fields + 1)
  }
  
  coding_box <- str_c("injq1", rep, sep="_")
  num_used_coding_boxes <-
    if_else(is.na(pull(form, coding_box)), 0, 1)
  
  return(
    select(form, c(region, site, studyid, ptinit, ptstatus,
                   oth_upper, oth_spine, oth_lower, oth_pelvis, coding_box)) %>%
      filter(ptstatus == 1 &
               (is_text_field_invalid |
                  if_else(num_used_text_fields == 0,
                          num_used_coding_boxes != 0,
                          num_used_coding_boxes < num_used_text_fields))) %>%
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
  group1 <- str_c(c("disloc", "othdloc", "injq2"), rep, sep="_")
  problems <- check_invalid_boxes(form, 12, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 2 of form5.2/6/10 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("locoth", "othloctx", "injq31", "injq312"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:4], 
                                  "Textfield and coding box 3 \"other locations of treatment\" of form5.2/6/10 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("othtx", "othtxsp", "injq321", "injq322", "injq323"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:5], 
                                  "Textfield and coding box 3 \"other treatment\" of form5.2/6/10 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("howstab", "othstab", "injq41", "injq42", "injq43"), rep, sep="_")
  problems <- check_invalid_boxes(form, 6, group1[1], group1[2], group1[3:5], 
                                  "Textfield and coding box 4 of form5.2/6/10 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("oifix", "oifixsp", "injq51o", "injq52o"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:4], 
                                  "Textfield and coding box 5 \"other internal fixation\" of form5.3/7/11 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("othnop", "othnopsp", "injq51no", "injq52no"), rep, sep="_")
  problems <- check_invalid_boxes(form, 1, group1[1], group1[2], group1[3:4], 
                                  "Textfield and coding box 5 \"other non-operative treatment\" of form5.3/7/11 is completed if and only if the other option is chosen")
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
  group1<- str_c(c("add", "othaddsp", "injq7a"), rep, sep="_")
  problems <- check_invalid_boxes(form, 6, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 7 \"other solution additive\" of form5.4/8/12 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("delivery", "othdelsp", "injq7b"), rep, sep="_")
  problems <- check_invalid_boxes(form, 6, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 7 \"Other delivery method\" of form5.4/8/12 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("failreas", "othflsp", "injq8"), rep, sep="_")
  problems <- check_invalid_boxes(form, 9, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 8 of form5.4/8/12 is completed if and only if the other option is chosen")
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
  group1 <- str_c(c("delreas", "othdlysp", "injq9"), rep, sep="_")
  problems <- check_invalid_boxes(form, 9, group1[1], group1[2], group1[3], 
                                  "Textfield and coding box 9 of form5.4/8/12 is completed if and only if the other option is chosen")
  return (problems)
}

#' Filters out invalid rows for box 1a "Other solution" of form5.14
#' 
#' @param form a form containing ptstatus and form5.14
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.14_box1a <- function(form) {
  check_invalid_boxes(form, c(1, 1, 1),
                      c("iosoln1", "iosoln2", "iosoln3"),
                      c("iosolns1", "iosolns2", "iosolns3"),
                      c("esolcd1", "esolcd2", "esolcd3"),
                      "Textfield and coding box 1a \"Other solution\" of form5.14 is completed if and only if the other option is chosen")
}

#' Filters out invalid rows for box 1b "Other time of application" of form5.14
#' 
#' @param form a form containing ptstatus and form5.14
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.14_box1b <- function(form) {
  check_invalid_boxes(form, c(3, 3, 3),
                      c("applied1", "applied2", "applied3"),
                      c("applids1", "applids2", "applids3"),
                      c("wappcd1", "wappcd2", "wappcd3"),
                      "Textfield and coding box 1b \"Other time of application\" of form5.14 is completed if and only if the other option is chosen")
}

#' Filters out invalid rows for box 2 of form5.14
#' 
#' @param form a form containing ptstatus and form5.14
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.14_box2 <- function(form) {
  check_invalid_boxes(form, c(1, 1, 1),
                      c("odres1", "odres2", "odres3"),
                      c("odress1", "odress2", "odress3"),
                      c("drescd1", "drescd2", "drescd3"),
                      "Textfield and coding box 2 of form5.14 is completed if and only if the other option is chosen")
}

#' Filters out invalid rows for box 3 of form5.14
#' 
#' @param form a form containing ptstatus and form5.14
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form5.14_box3 <- function(form) {
  check_invalid_boxes(form, c(1, 1, 1),
                      c("sosoln1", "sosoln2", "sosoln3"),
                      c("sosolns1", "sosolns2", "sosolns3"),
                      c("ssolcd1", "ssolcd2", "ssolcd3"),
                      "Textfield and coding box 3 of form5.14 is completed if and only if the other option is chosen")
}

#' Filters out invalid rows for box 1 "Other patient discharge" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box1a <- function(form) {
  return (check_invalid_boxes(form, 6, "dcto", "othdcto", "p19q1dc",
                              "Textfield and coding box 1 \"Other patient discharge\" of form6.1 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 1 "Cause of death" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box1b<- function(form) {
  return (check_invalid_boxes(form, 1, "deceased", "cause", c("p19q1dth", "p19q1dt2"), 
                              "Textfield and coding box 1 \"Cause of death\" of form6.1 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 2 "Other complication" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box2a <- function(form) {
  return (check_invalid_boxes(form, 1,"othnoc", "othnocsp", c("p19q2a","p19q2a2"), 
                              "Textfield and coding box 2 \"Other complication\" of form6.1 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 2 "Other cardiac complication" of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' 
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box2b <- function(form) {
  return (check_invalid_boxes(form, 1, "ccoth", "ccothsp", "p19q2b", 
                              "Textfield and coding box 2 \"Other cardiac complication\" of form6.1 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 4 of form6.1
#' 
#' @param form a form containing ptstatus and form6.1
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form6.1_box4 <- function(form) {
  return (check_invalid_boxes(form, 5,"howcmpl", "othcmpl", "p19q4",
                              "Textfield and coding box 4 of form6.1 is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 3 of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box3 <- function(form, rep) {
  group1 <- str_c(c("typecmpl", "othcmpl", "cmpq3"),rep ,sep="~")
  return (check_invalid_boxes(form, 14, group1[1], group1[2], group1[3],
                              "Textfield and coding box 3 of form7.x is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 4 "Other treatment" of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box4a <- function(form, rep) {
  group1 <- str_c(c("mancomp", "manoth", "cmpq4a"), rep ,sep="~")
  return (check_invalid_boxes(form, 3, group1[1], group1[2], group1[3],
                              "Textfield and coding box 4 \"Other treatment\" of form7.x is completed if and only if the other option is chosen"))
}

#' Filters out invalid rows for box 4 "Other non-operative treatment" of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box4b <- function(form, rep) {
  group1 <- str_c(c("mother", "mothersp", "cmpq4b"), rep ,sep="~")
  return (check_invalid_boxes(form, 1, group1[1], group1[2], group1[3],
                              "Textfield and coding box 4 \"Other non-operative treatment\" of form7.x is completed if and only if the other option is chosen"))
}


#' Filters out invalid rows for box 4 "Other details" of form7.x
#' 
#' @param form a form containing ptstatus and form7.x
#' @param rep the repetition/iteration of the current form
#' @return a data frame containing all the invalid rows
#' 
#' @import tidyverse
#' @export
check_form7.x_box4c <- function(form, rep) {
  group1 <- str_c(c("nother", "nothersp", "cmpq4c"), rep ,sep="~")
  return (check_invalid_boxes(form, 1, group1[1], group1[2], group1[3],
                              "Textfield and coding box 4 \"Other details\" of form7.x is completed if and only if the other option is chosen"))
}