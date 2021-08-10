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
        (ptstatus == 2 & is.na(parse_dmY(condate)))
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

#' Check that all entries in form3.2 are filled with valid values
#' 
#' @param form dataframe containing form3.2
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form3.2 <- function(form) {
  form %>%
    mutate(comment="Invalid or missing entries") %>%
    filter(ptstatus == 1 &
             (is_invalid_na_or_n(placeinj) | placeinj == 0 |
                is_invalid_or_n(othplace) |
                is_invalid_na_or_n(transfus) |  transfus == 0 |
                is_invalid_or_n(transoth) |
                is_invalid_or_na(transnot) | (transfus == 2 & is_n(transnot)) |
                
                (is_invalid_na_or_n(nonorth) | nonorth == 0 |
                   is_invalid_or_na(chest) | is_invalid_or_na(pneumot) |  
                   is_invalid_or_na(rib) | is_invalid_or_na(hemopneu) | 
                   is_invalid_or_na(hvasc) | is_invalid_or_na(contbr) |
                   is_invalid_or_na(othchest) | is_invalid_or_n(chestsp) |
                   
                   is_invalid_or_na(abdo) | is_invalid_or_na(spleen) |
                   is_invalid_or_na(liver) | is_invalid_or_na(lbowel) | 
                   is_invalid_or_na(sbowel) | is_invalid_or_na(urethra) | 
                   is_invalid_or_na(bladder) | is_invalid_or_na(kidney) |
                   is_invalid_or_na(othabdo) | is_invalid_or_n(abdosp) |
                   
                   is_invalid_or_na(hdneck) | is_invalid_or_na(majfacl) |
                   is_invalid_or_na(minfacl) | is_invalid_or_na(faclfrac) |
                   is_invalid_or_na(concuss) | is_invalid_or_na(icbleed) | 
                   is_invalid_or_na(minhead) | is_invalid_or_na(sklfrac) | 
                   is_invalid_or_na(othhn) | is_invalid_or_n(hdnecksp) |
                   
                   is_invalid_or_na(burn) |
                   is_invalid_or_na(burnsev) | is_invalid_or_na(sarea) |
                   
                   (nonorth == 1 &
                      (is_n(chest) | is_n(abdo) | is_n(hdneck) | is_n(burn) |
                         (chest == 1 &
                            (is_n(pneumot) | is_n(rib) |
                               is_n(hemopneu) |is_n(hvasc) |
                               is_n(contbr) | is_n(othchest) |
                               (pneumot == 1) + (rib == 1) +
                               (hemopneu == 1) + (hvasc == 1) +
                               (contbr == 1) + (othchest == 1) == 0)) |
                         
                         (abdo == 1 &
                            (is_n(spleen) | is_n(liver) | 
                               is_n(lbowel) | is_n(sbowel) | 
                               is_n(urethra) | is_n(bladder) |
                               is_n(kidney) | is_n(othabdo) |
                               (spleen == 1) + (liver == 1) +
                               (lbowel == 1) + (sbowel == 1) +
                               (urethra == 1) + (bladder == 1) +
                               (kidney == 1) + (othabdo == 1) == 0)) |
                         
                         (hdneck == 1 &
                            (is_n(majfacl) | is_n(minfacl) |
                               is_n(faclfrac) | is_n(concuss) |
                               is_n(icbleed) | is_n(minhead) |
                               is_n(sklfrac) | is_n(othhn) |
                               (majfacl == 1) + (minfacl == 1) +
                               (faclfrac == 1) + (concuss == 1) +
                               (icbleed == 1) + (minhead == 1) +
                               (sklfrac == 1) + (othhn == 1) == 0)) |
                         
                         (burn == 1 & (is_n(burnsev) | burnsev == 0 |
                                         is_n(sarea) | sarea == 0))))) |
                
                is_invalid_na_or_n(northinj) | northinj == 0))
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