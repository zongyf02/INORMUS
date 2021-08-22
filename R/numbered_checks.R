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

#' Check that the response to I&D is consistent with whether the fracture is open or closed 
#' in one set of form5.x are related
#' 
#' @param form dataframe containing ptstatus and one set of form5.x
#' @param rep which set of form 5.x
#' @return a dataframe containing problematic entries with relevant columns
#' @import tidyverse
#' @export 
check_openclos_iandd <- function(form, rep){
  openclos <- pull(form, str_c("openclos", rep, sep = "_"))
  iandd <- pull(form, str_c("iandd", rep, sep = "_"))
  
  problems <- form %>% transmute(
    region, site, studyid, ptinit, ptstatus,  openclos, iandd,
    comment = "The response to I&D should be consistent with whether the fracture is open or closed ") %>% 
    filter(ptstatus == 1 & 
             (openclos == 1 & iandd == 3) |
             (openclos == 2 & (iandd == 1 | iandd == 2)))
  return(problems)
}

#' Check that details on the patient's surgery is consistent
#' 
#' @param form dataframe containing ptstatus, form3.2, form5.3, and form5.4
#' @param rep which set of form 5.3x
#' @return a dataframe containing problematic entries with relevant columns
#' 
#' @import tidyverse
#' @export 
check_operat_failsurg_delsurg <- function(form, rep) {
  operat <- pull(form, str_c("operat", rep, sep = "_"))
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
  
#' Check that closed fracture injuries have have NA selected in form5.14 
#' 
#' @param form a dataframe containing form1.1, form5.1x, form5.14
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_openclos_NA <- function(form) {
  return (form %>% 
            transmute(
              region, site, studyid, ptinit, ptstatus,openclos_1, openclos_2,
              openclos_3, naprep1, naprep2, naprep3, 
              comment ="On question 1 of form5.14, Not Applicable should be selected for closed fracture injuries") %>% 
            filter(ptstatus == 1 & 
                     ((openclos_1 == 2 & naprep1 != 1) | 
                        (openclos_2 == 2 & naprep2 != 1) | 
                        (openclos_3 == 2 & naprep3 != 1))))
}

#' Check that consent date is before or on discharge date or date of death
#' 
#' @param form a dataframe containing form1.1 and form6.1
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_condate_hdcdate_dthdate <- function(form){
  problems <- form %>% transmute(
    region, site, studyid, ptinit, ptstatus, condate,
    parsed_condate = parse_dmY(condate), dchosp, hdcdate, deceased, dthdate,
    comment = "Consent date should be before or on discharge date or date of death") %>%
    filter(ptstatus == 1 &
             ((dchosp == 1 & parsed_condate > parse_dmY(hdcdate)) |
             (deceased == 1 & parsed_condate > parse_dmY(dthdate)))) %>%
    mutate(parsed_condate = NULL)
  return(problems)
}