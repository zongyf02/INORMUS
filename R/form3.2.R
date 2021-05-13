#' Read in form3.2
#'
#' Read in form3.2 from file path
#'
#' @param path path to file
#' @param raw if TRUE, return raw data
#' @return form3.2 dataframe
#' @export
#' @import tidyverse
read_form3.2 <- function(path, raw = FALSE) {
  form <-read_delim(
    path, delim = "|",
    col_types = "------iccccccccccccccccccccccccccccccccccccccccccccccc----")

  if (!raw) {
    form <- form %>%
      mutate(across(c(3, 5:6, 8:15, 17:25, 27:35, 37:48), parse_int),
             region = parse_region(studyid),
             site = parse_site(studyid)) %>%
      relocate(c(region, site), .before = studyid)
  }

  return(form)
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
  return(check_form_box_I(form, 9, "placeinj", "othplace", "p5q5"))
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