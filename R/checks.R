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


#' Check that consent date should be on the same day, or after the date of injury
#' 
#' condate after injdate 0-92 days
#' 
#' @param form a dataframe containing form1.1 and form3.1
#' @return a dataframe containing problematic entries
#' 
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
    filter(ptstatus == 1 & (date_diff < 0 | date_diff > 92))
  
  return(problems)
}

#' Check that hospital admission date should be on the same day,
#' or after the date of injury
#' 
#' hpsdate after injdate 0-92 days
#' 
#' @param form a dataframe containing form1.1, form3.1, and form4.1
#' @return a dataframe containing problematic entries
#' 
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
    filter(ptstatus == 1 & (diff_date < 0 | diff_date > 92))
  
  return(problems)
}

#' Check that consent date should be 0 - 30 days after hospital admission date
#' 
#' condate after hspdate 0-30 days
#' 
#' @param form a dataframe containing form1.1 and form4.1
#' @return a dataframe containing problematic entries
#' 
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
    filter(ptstatus == 1 & (diff_date < 0 | diff_date > 30))
  
  return(problems)
}

#' Check that time from injury to hsp admission should be within +/- 24 hrs
#' range of difference between injdate and hspdate
#' 
#' @param form a dataframe containing form1.1, form3.1, form4.1
#' @return a dataframe containing problematic entries
#' 
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
    filter(ptstatus == 1 &
             ((ihunits == 1 &
                 ((ihhrs < (date_diff - 24)) | (ihhrs > (date_diff + 24)))) |
                (ihunits == 2 &
                   ((ihdays * 24 < (date_diff - 24)) | (ihdays * 24 > (date_diff + 24))))))
  
  return(problems)
}

#' Check that the number of orthopedic injuries stated on form 3.2 is consistent 
#' with the number of sets of injury forms completed
#' 
#' @param form dataframe containing pststatus, form 3.2, and form5.x 
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export 
check_northinj_form5.x <- function(form) {
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
    filter(ptstatus == 1 &
             ((northinj != 1 | is_set1_empty | !is_set2_empty | !is_set3_empty
               | continue_1 != 0) &
                (northinj != 2 | is_set1_empty | is_set2_empty | !is_set3_empty |
                   continue_1 != 1 | continue_2 != 0) &
                ((northinj != 3 & northinj != 4) | is_set1_empty | is_set2_empty
                 | is_set3_empty | continue_1 != 1 | continue_2 != 1)))
  return (problems)
}

#' Check that the number of orthopedic injuries stated on form 3.2 is consistent 
#' with the Wound & Skin Prep form 5.14
#' 
#' @param form a dataframe containing pststatus, form 3.2, and form 5.14
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export 
check_northinj_form5.14 <- function(form) {
  form %>%
    transmute(
      region, site, studyid, ptinit, ptstatus,
      northinj, noinj2, noinj3,
      comment = "The number of orthopedic injuries stated on form 3.2 isn't consistent with the Wound & Skin Prep form 5.14") %>%
    filter(ptstatus == 1 &
             ((northinj != 1 | noinj2 != 1 | noinj3 != 1) &
                (northinj != 2 | noinj2 != 0 | noinj3 != 1) &
                ((northinj != 3 & northinj != 4) | noinj2 != 0 | noinj3 != 0)))
}

#' The time from injury to hospital admission should be within 24 hours
#' if the patient is coming from the Accident/Injury Site 
#' 
#' @param form a dataframe containing ptstatus and form4.1
#' @return a dataframe containing problematic entries with relevant columns
#' 
#' @import tidyverse
#' @export
check_admfrom_ihunits <- function(form) {
  problems <- transmute(form,
                        region, site, studyid, ptinit, ptstatus, admfrom, ihunits, ihhrs, ihdays,
                        comment = "The time from injury to hospital admission should be within 24 hours if the patient is coming from the Accident/Injury Site") %>%
    filter(ptstatus == 1 &
             (admfrom  == 1 & (ihunits != 1 | (ihunits == 1 & ihhrs > 24))))
  return(problems)
}

#' Check that the location of fracture and the location of dislocation in one
#' set of form5.x are related
#' 
#' @param form dataframe containing ptstatus and one set of form5.x
#' @param rep which set of form5.x is checked
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export 
check_fracwith_diswith <- function(form, rep) {
  upper <- str_c(c("lclav","rclav","lscap","rscap", "lphum", "rphum", "lmhum",
                   "rmhum", "ldhum", "rdhum", "lolec", "rolec", "lprad",
                   "rprad", "lmrad", "rmrad", "ldrad", "rdrad", "lpuln","rpuln",
                   "lmuln", "rmuln", "lduln", "rduln", "lothup", "rothup"),
                 rep, sep = "_")
  upper_fracture <- FALSE
  for (fracture in upper) {
    upper_fracture <- pull(form, fracture) == 1 | upper_fracture
  }
  
  spine <- str_c(c("lcerv", "rcerv", "lthor", "rthor", "llumb", "rlumb",
                   "lothspin", "rothspin"), rep, sep = "_")
  spine_fracture <- FALSE
  for (fracture in spine) {
    spine_fracture <- pull(form, fracture) == 1 | spine_fracture
  }
  
  lower <- str_c(c("lpfem", "rpfem", "lmfem", "rmfem", "ldfem", "rdfem", "lpat",
                   "rpat", "lptib", "rptib", "lmtib", "rmtib", "ldtib", "rdtib",
                   "lfib", "rfib", "lankp", "rankp", "lankm", "rankm","ltalus",
                   "rtalus","lcalc", "rcalc", "lfoot", "rfoot", "lothlo",
                   "rothlo"), rep, sep = "_")
  lower_fracture <- FALSE
  for (fracture in lower) {
    lower_fracture <- pull(form, fracture) == 1 | lower_fracture
  }
  
  pelvis <- str_c(c("lpfem", "rpfem", "lmfem", "rmfem", "ldfem", "rdfem",
                    "lacet","racet", "lsacro", "rsacro", "lsacrum", "rsacrum",
                    "liwing", "riwing", "lpsymph", "rpsymph", "lramus","rramus",
                    "lothpelv", "rothpelv"), rep, sep = "_")
  pelvis_fracture <- FALSE
  for (fracture in pelvis) {
    pelvis_fracture <- pull(form, fracture) == 1 | pelvis_fracture
  }
  
  disloc_vector <- pull(form, str_c("disloc", rep, sep = "_"))
  injq2_vector <- pull(form, str_c("injq2", rep, sep = "_"))
  location_of_dislocation <-
    case_when(
      disloc_vector %in% c(4, 5, 6, 8, 10) ~ "U",
      disloc_vector %in% c(1, 2, 3) ~ "L",
      disloc_vector %in% c(9) ~ "S",
      disloc_vector %in% c(7, 11) ~ "P",
      disloc_vector == 12 ~
        case_when(
          injq2_vector %in% c(1, 3, 4) ~ "U",
          injq2_vector == 2 ~ "P",
          injq2_vector == 5 ~ "L"
        )
    )
  
  problems <- form %>% 
    transmute(
      region, site, studyid, ptinit, ptstatus,
      fracwith = eval(parse(text = str_c("fracwith_", rep))), upper_fracture,
      spine_fracture, lower_fracture, pelvis_fracture,
      diswith = eval(parse(text = str_c("diswith_", rep))),
      location_of_dislocation = location_of_dislocation,
      comment = "The location of fracture and the location of dislocation in one set of forms should be related") %>% 
    filter(ptstatus == 1 & fracwith == 1 & diswith == 1 & case_when(
      location_of_dislocation == "U" ~ !upper_fracture,
      location_of_dislocation == "L" ~ !lower_fracture,
      location_of_dislocation == "P" ~ !pelvis_fracture,
      location_of_dislocation == "S" ~ !spine_fracture))
  
  return(problems)
}

#' Check that the time from injury to the first antibiotic administration must
#' be consistent with the location of the first administration 
#' 
#' @param form dataframe containing ptstatus, form 4.1, and form5.3
#' @return a dataframe containing problematic entries with relevant columns
#' @import tidyverse
#' @export 
check_locabx <- function(form) {
  problems <- form %>% 
    transmute(
      region, site, studyid, ptinit, ptstatus, locabx, abxprior_1,
      inj_to_hsp = if_else(ihunits == 1, ihhrs,
                           if_else(ihunits == 2, ihdays * 24, as.numeric(NA))),
      inj_to_abx = if_else(iaunits == 1, iahrs,
                           if_else(iaunits == 2,  iadays * 24, as.numeric(NA))),
      hsp_to_stab = if_else(hsunits_1 == 1, ishrs_1,
                            if_else(hsunits_1 == 2, isdays_1 * 24, as.numeric(NA))),
      diff_inj_to_abx_inj_to_hsp = inj_to_abx - inj_to_hsp,
      diff_inj_to_abx_inj_to_stab = inj_to_abx - (inj_to_hsp + hsp_to_stab),
      comment = "The time from injury to the first antibiotic administration must be consistent with the location of the first administration") %>% 
    filter(ptstatus == 1 &
             (locabx == 1 &
                diff_inj_to_abx_inj_to_hsp < -24 &
                diff_inj_to_abx_inj_to_hsp > 0) &
             ((locabx == 2 | locabx == 3) &
                diff_inj_to_abx_inj_to_stab < -24 &
                diff_inj_to_abx_inj_to_stab > 0) &
             (locabx == 2 & abxprior_1 != 1) &
             (locabx == 4 |
                diff_inj_to_abx_inj_to_stab > 24 &
                diff_inj_to_abx_inj_to_stab < 0)) %>%
    mutate(diff_inj_to_abx_inj_to_hsp = NULL,
           diff_inj_to_abx_inj_to_stab = NULL)
  
  return(problems)
}

#' Check that details on the patient's surgery is consistent
#' 
#' @param form dataframe containing ptstatus, form3.2, form5.3, and form5.4
#' @param rep which set of form
#' @return a dataframe containing problematic entries with relevant columns
#' 
#' @import tidyverse
#' @export 
check_operat_failsurg_delsurg <- function(form, rep) {
  operat <-  pull(form, str_c("operat", rep, sep = "_"))
  failsurg <- pull(form, str_c("failsurg", rep, sep = "_"))
  delsurg <- pull(form, str_c("delsurg", rep, sep = "_"))
  
  problems = form %>%
    transmute(
      region, site, studyid, ptinit, ptstatus, northinj,
      operat, failsurg, delsurg,
      comment = "The patient's surgery information must be consistent") %>%
    filter(ptstatus == 1 & rep <= northinj &
             ((operat == 1 & (failsurg == 3 | delsurg == 3)) |
                (operat == 0 & !(failsurg == 3 & delsurg == 3)) |
                (failsurg == 1 & delsurg == 1)))
  
  return(problems)
}

#' Check that all entries in form1.1 are filled with valid values
#' 
#' @param form dataframe containing form1.1
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form1.1 <- function(form) {
  form %>% 
    filter(
      is_invalid_na_or_n(fracdis) | fracdis == 0 |
        is_invalid_na_or_n(acute3m) | acute3m == 0 |
        is_invalid_na_or_n(willing) | willing == 0 |
        is_invalid_na_or_n(comply) | comply == 0 |
        is_invalid_na_or_n(ptstatus) | ptstatus == 0 |
        (ptstatus == 1 & is_n(condate)) |
        (ptstatus == 2 & is_na(parse_dmY(condate)))
    )
}

#' Check that all entries in form2.1 are filled with valid values
#' 
#' @param form dataframe containing form2.1
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form2.1 <- function(form){
  problems <- form %>% 
    filter(ptstatus == 1 &
             (is_invalid_na_or_n(age) | age < 18 |
                is_invalid_na_or_n(sex) | sex == 0 |
                is_invalid_na_or_n(literate) | literate == 0 |
                is_invalid_na_or_n(educ) | educ == 0 |
                is_invalid_na_or_n(occup) | occup == 0 |
                is_invalid_or_n(othoccup) |
                is_invalid_na_or_n(income) | income == 0 |
                is_invalid_na_or_n(locat) | locat == 0 |
                is_invalid_na_or_n(smoking) | smoking == 0 |
                is_invalid_na_or_n(hlthins) | hlthins == 0)) %>%
    mutate(comment="Invalid or missing entries")
  return(problems)
}

#' Check that all entries in form2.2 are filled with valid values
#' 
#' @param form dataframe containing form1.1 and form2.2
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form2.2 <- function(form) {
  problems <- form %>%
    mutate(num_comorb = as.numeric(ischhrt == 1) +
             as.numeric(cvascd == 1) +
             as.numeric(lowresp == 1) +
             as.numeric(cancer == 1) +
             as.numeric(diabetes == 1) +
             as.numeric(copd == 1) + 
             as.numeric(htn == 1) + 
             as.numeric(hivaids == 1) + 
             as.numeric(gidisord == 1) +
             as.numeric(anembld == 1) +
             as.numeric(tb == 1) +
             as.numeric(pneum_2.2 == 1) +
             as.numeric(malaria == 1) +
             as.numeric(asthma == 1) +
             as.numeric(osteo == 1) +
             as.numeric(othercm == 1)) %>%
    filter(ptstatus == 1 &
             (is_invalid_na_or_n(nonecm) |
                is_invalid_na_or_n(ischhrt) |
                is_invalid_na_or_n(cvascd) |
                is_invalid_na_or_n(lowresp) |
                is_invalid_na_or_n(cancer) |
                is_invalid_na_or_n(diabetes) |
                is_invalid_na_or_n(copd) |
                is_invalid_na_or_n(htn) |
                is_invalid_na_or_n(hivaids) |
                is_invalid_na_or_n(gidisord) |
                is_invalid_na_or_n(anembld) |
                is_invalid_na_or_n(tb) |
                is_invalid_na_or_n(pneum_2.2) |
                is_invalid_na_or_n(malaria) |
                is_invalid_na_or_n(asthma) |
                is_invalid_na_or_n(osteo) |
                is_invalid_na_or_n(othercm) |
                is_invalid_or_n(comorb) |
                (nonecm == 0 & num_comorb <= 0) |
                (nonecm == 1 & num_comorb > 0))) %>%
    mutate(num_comorb = NULL,
           comment = "Invalid or missing entries")
  return(problems)
}
#' Check that all entries in form3.1 are filled with valid values
#' 
#' @param form dataframe containing form1.1 and form3.1
#' @param checkIntent logical True to check question 4.1 Intent of Injury
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form3.1 <- function(form, checkIntent = FALSE) {
  problems <- form %>% 
    filter(ptstatus == 1 &
             (is.na(parse_dmY(injdate)) |
                is_invalid_na_or_n(alcohol) | alcohol == 0 |
                is_invalid_na_or_n(selfi) | selfi == 0 |
                # transport column
                is_invalid_na_or_n(trans) |
                is_invalid_or_na(transsp) | 
                is_invalid_or_na(bhelm) | is_invalid_or_na(mhelm) |
                is_invalid_or_na(tbsbelt) | is_invalid_or_na(asbelt) |
                is_invalid(othtrans) |
                (trans == 1 &
                   (is_n(transsp) | transsp == 0 | is_n(othtrans) |
                      (transsp == 6 & (is_n(bhelm) | bhelm == 0)) |
                      (transsp == 7 & (is_n(mhelm) | mhelm == 0)) |
                      (transsp == 8 & (is_n(tbsbelt) | tbsbelt == 0)) |
                      (transsp == 9 & (is_n(asbelt) | asbelt == 0)))) |
                # fall column
                is_invalid_na_or_n(fall) |
                is_invalid_or_na(fallfrom) | 
                is_invalid_or_na(lowhigh) |
                (fall == 1 &
                   (is_n(fallfrom) | fallfrom == 0 |
                      (fallfrom == 2 & (is_n(lowhigh) | lowhigh == 0)))) |
                # intentional column
                is_invalid_na_or_n(intent) |
                is_invalid_or_na(intentsp) |
                is_invalid(othinten) |
                (intent == 1 & (is_n(intentsp) | intentsp == 0 |
                                  is_n(othinten))) |
                # struck/lifting column
                is_invalid_na_or_n(strklift) |
                is_invalid_or_na(stliftsp)  |
                is_invalid(othstlif) |
                (strklift == 1 & (is_n(stliftsp) | stliftsp == 0
                                  | is_n(othstlif))) |
                # other column
                is_invalid_na_or_n(othmech) |
                is_invalid_or_na(omechsp) | 
                is_invalid(othmoth) |
                (othmech == 1 & (is_n(omechsp) | omechsp == 0 |
                                   is_n(othmoth))) |
                # number of injuries should be one
                (trans == 1) + (fall == 1) + (intent == 1) + (strklift == 1) +
                (othmech == 1) != 1 |
                # Intent of injury
                (checkIntent &
                   (is_invalid_na_or_n(intinj) | intinj == 0 |
                      is_invalid_or_na(intentof) |
                      (intinj == 2 & is_n(intentof)))))) %>%
    mutate(comment="Invalid or missing entries")
  return(problems)
}

#' Check that all entries in form4.1 are filled with valid values
#' 
#' @param form dataframe containing form1.1 and form4.1
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form4.1 <- function(form) {
  problems <- form %>% 
    filter(ptstatus == 1 & 
             (is.na(parse_dmY(hspdate)) |
                is_invalid_na_or_n(admfrom) | admfrom == 0 |
                is_invalid_or_n(othfrom) |
                is_invalid_na_or_n(transto) | transto == 0 |
                is_invalid_or_n(othto) |
                is_invalid_na_or_n(ihunits) | ihunits == 0 |
                is_invalid_or_n(ihhrs) | is_invalid_or_n(ihdays) |
                is_invalid_or_na(rsdelay) | rsdelay == 0 |
                is_invalid_or_n(othdelay) |
                (((ihunits == 1 & ihhrs > 24) | ihunits == 2) &
                   is_n(rsdelay)) |
                # Q6
                is_invalid_na_or_n(abx) | abx == 0 |
                is_invalid_or_na(iaunits) | iaunits == 0 |
                is_invalid(iahrs) | is_invalid(iadays) |
                is_invalid_or_na(locabx) | locabx == 0 |
                is_invalid_or_na(injscene) | is_invalid_or_na(erinhosp) |
                is_invalid_or_na(preop) | is_invalid_or_na(oper) |
                is_invalid_or_na(postop) | is_invalid_or_na(dnradabx) |
                (abx == 1 &
                   (is_n(iaunits) | is_n(iahrs) | is_n(iadays) |
                      is_n(locabx) |
                      is_n(injscene) | is_n(erinhosp) |
                      is_n(preop) | is_n(oper) | 
                      is_n(postop) | is_n(dnradabx) |
                      (injscene + erinhosp + preop + oper + postop +
                         dnradabx == 0) |
                      (dnradabx == 1 &
                         injscene + erinhosp + preop + oper + postop > 0))))
    ) %>%
    mutate(comment="Invalid or missing entries")
  return(problems)
}

#' Check that all entries in form3.2 are filled with valid values
#' 
#' @param form dataframe containing form3.2
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export

check_invalid_form3.2 <- function(form) {
  return(form %>% 
           filter(
             ((ptstatus == 1) &
                (northinj == 0 | is_invalid_na_or_n(northinj))) |
               ((ptstatus == 1) & 
                  (is_invalid_na_or_n(nonorth) | 
                     nonorth == 0 |
                     (chest == 1 & (is_invalid_na_or_n(pneumot) |  
                                      is_invalid_na_or_n(rib) |
                                      is_invalid_na_or_n(hemopneu) | 
                                      is_invalid_na_or_n(hvasc) | 
                                      is_invalid_na_or_n(contbr) |
                                      is_invalid_na_or_n(othchest) | 
                                      ((pneumot == 1) + (rib == 1) +
                                         (hemopneu == 1) + (hvasc == 1) +
                                         (contbr == 1) + (othchest == 1) == 0))) |
                     
                     (abdo == 1 &  (is_invalid_na_or_n(spleen) |
                                      is_invalid_na_or_n(liver) | 
                                      is_invalid_na_or_n(lbowel) | 
                                      is_invalid_na_or_n(sbowel) | 
                                      is_invalid_na_or_n(urethra) | 
                                      is_invalid_na_or_n(bladder) |
                                      is_invalid_na_or_n(kidney) |
                                      is_invalid_na_or_n(othabdo) | 
                                      ((spleen == 1) + (liver == 1) +
                                         (lbowel == 1) + (sbowel == 1) +
                                         (urethra == 1) + (bladder == 1) +
                                         (kidney == 1) + (othabdo == 1) == 0))) |
                     
                     (hdneck == 1 & (is_invalid_na_or_n(majfacl) |
                                       is_invalid_na_or_n(minfacl) |
                                       is_invalid_na_or_n(faclfrac) |
                                       is_invalid_na_or_n(concuss) |
                                       is_invalid_na_or_n(icbleed) | 
                                       is_invalid_na_or_n(minhead) |
                                       is_invalid_na_or_n(sklfrac) | 
                                       is_invalid_na_or_n(othhn) | 
                                       ((majfacl == 1) + (minfacl == 1) +
                                          (faclfrac == 1) + (concuss == 1) +
                                          (icbleed == 1) + (minhead == 1) +
                                          (sklfrac == 1) + (othhn == 1) == 0))) |
                     (burn == 1 & (burnsev == 0 | is_invalid_na_or_n(burnsev) | 
                                     (burnsev != 0 & (is_invalid_na_or_n(sarea) | sarea == 0)))))) |
               ((ptstatus == 1) &
                  (is_invalid_na_or_n(transfus) | 
                     transfus == 0 |
                     (transfus == 2 & is_invalid_na_or_n(transnot)))) | 
               ((ptstatus == 1) & 
                  (is_invalid_na_or_n(placeinj) | 
                     placeinj == 0))) %>%
           mutate(comment="Invalid or missing entries"))
}

#' Check that all entries in form5.2 are filled with valid values
#' 
#' @param form dataframe containing form1.1, form3.2, and form5.2
#' @param rep which set of form
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form5.2x <- function(form, rep){
  diswith <- pull(form, str_c("diswith", rep, sep = "_"))
  disloc <- pull(form, str_c("disloc", rep, sep = "_"))
  othdloc <- pull(form, str_c("othdloc", rep, sep = "_"))
  txprior <- pull(form, str_c("txprior", rep, sep = "_"))
  lohosp <- pull(form, str_c("lohosp", rep, sep = "_"))
  ltradhl <- pull(form, str_c("ltradhl", rep, sep = "_"))
  lnonhosp <- pull(form, str_c("lnonhosp", rep, sep = "_"))
  locoth <- pull(form, str_c("locoth", rep, sep = "_"))
  othloctx <- pull(form, str_c("othloctx", rep, sep = "_"))
  splint <- pull(form, str_c("splint", rep, sep = "_"))
  dressopn <- pull(form, str_c("dressopn", rep, sep = "_"))
  nostabil <- pull(form, str_c("nostabil", rep, sep = "_"))
  irrig <- pull(form, str_c("irrig", rep, sep = "_"))
  abxprior <- pull(form, str_c("abxprior", rep, sep = "_"))
  bandages <- pull(form, str_c("bandages", rep, sep = "_"))
  ostabil <- pull(form, str_c("ostabil", rep, sep = "_"))
  othtx <- pull(form, str_c("othtx", rep, sep = "_"))
  othtxsp <- pull(form, str_c("othtxsp", rep, sep = "_"))
  ptstabil <- pull(form, str_c("ptstabil", rep, sep = "_"))
  method <- pull(form, str_c("method", rep, sep = "_"))
  howstab <- pull(form, str_c("howstab", rep, sep = "_"))
  othstab <- pull(form, str_c("othstab", rep, sep = "_"))
  hspstab <- pull(form, str_c("hspstab", rep, sep = "_"))
  
  
  problems <- form %>% filter(
    ptstatus == 1 & rep <= northinj &
      (is_invalid_na_or_n(diswith) | diswith == 0 |
         is_invalid_or_na(disloc) | (diswith == 1 & is_n(disloc)) |
         is_invalid_or_n(othdloc) |
         is_invalid_na_or_n(txprior) | txprior == 0 |
         is_invalid_or_na(lohosp) | is_invalid_or_na(ltradhl) |
         is_invalid_or_na(lnonhosp) | is_invalid_or_na(locoth) |
         is_invalid(othloctx) |
         is_invalid_or_na(splint) | is_invalid_or_na(dressopn) |
         is_invalid_or_na(nostabil) | is_invalid_or_na(irrig) |
         is_invalid_or_na(abxprior) | is_invalid_or_na(bandages) |
         is_invalid_or_na(ostabil) | is_invalid_or_na(othtx) |
         is_invalid(othtxsp) |
         (txprior == 1 & 
            (is_n(lohosp) | is_n(ltradhl) | is_n(lnonhosp) | is_n(locoth) |
               is_n(othloctx) |
               is_n(splint) | is_n(dressopn) | is_n(nostabil) | is_n(irrig) |
               is_n(abxprior) | is_n(bandages) | is_n(ostabil) | is_n(othtx) |
               is_n(othtxsp))) |
         is_invalid_na_or_n(ptstabil) | ptstabil == 0 |
         is_invalid_or_na(method) | is_invalid_or_na(howstab) |
         is_invalid(othstab) | is_invalid_or_na(hspstab) |
         (ptstabil == 1 & 
            (is_n(method) | is_n(howstab) | is_n(othstab) | is_n(hspstab))))) %>%
    mutate(comment="Invalid or missing entries")
  return(problems)
}

#' Check that initials between forms are consistent
#' 
#' @param list list of forms to check for intial consistencies
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_initials <- function(list) {
  for (i in 1:length(list)) {
    form <- list[[i]] %>% select("region", "site", "studyid", "ptinit")
    if (i == 1) {
      merged <- form
    } else {
      merged <- full_join(merged, form, by = c("region", "site", "studyid"),
                          suffix = c(str_c("(", i - 1, ")"),
                                     str_c("(", i, ")")))
    }
  }
  
  cols <- str_extract(colnames(merged), ".*\\(\\d*\\).*")
  cols <- cols[!is.na(cols)]
  cond <- FALSE
  i <- 1
  end <- length(cols)
  while (i < end) {
    cond <- cond | (pull(merged, cols[i]) != pull(merged, cols[i + 1]))
    i <- i + 1
  }
  
  merged %>%
    transmute(region, site, studyid, comment = "Inconsistent initials") %>%
    filter(cond)
}