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

#' Check that all entries in form5.1x are filled with valid values
#' 
#' @param form dataframe containing form1.1, form3.2, and form5.1x
#' @param rep which set of form
#' @return a dataframe containing problematic entries
#' 
#' @import tidyverse
#' @export
check_invalid_form5.1x <- function(form, rep){
  fracwith <- pull(form, str_c("fracwith", rep, sep = "_"))
  openclos <- pull(form, str_c("openclos", rep, sep = "_"))
  ggrade <- pull(form, str_c("ggrade", rep, sep = "_"))
  lclav <- pull(form, str_c("lclav", rep, sep = "_"))
  rclav <- pull(form, str_c("rclav", rep, sep = "_"))
  lscap <- pull(form, str_c("lscap", rep, sep = "_"))
  rscap <- pull(form, str_c("rscap", rep, sep = "_"))
  lphum <- pull(form, str_c("lphum", rep, sep = "_"))
  rphum <- pull(form, str_c("rphum", rep, sep = "_"))
  lmhum <- pull(form, str_c("lmhum", rep, sep = "_"))
  rmhum <- pull(form, str_c("rmhum", rep, sep = "_"))
  ldhum <- pull(form, str_c("ldhum", rep, sep = "_"))
  rdhum <- pull(form, str_c("rdhum", rep, sep = "_"))
  lolec <- pull(form, str_c("lolec", rep, sep = "_"))
  rolec <- pull(form, str_c("rolec", rep, sep = "_"))
  lprad <- pull(form, str_c("lprad", rep, sep = "_"))
  rprad <- pull(form, str_c("rprad", rep, sep = "_"))
  lmrad <- pull(form, str_c("lmrad", rep, sep = "_"))
  rmrad <- pull(form, str_c("rmrad", rep, sep = "_"))
  ldrad <- pull(form, str_c("ldrad", rep, sep = "_"))
  rdrad <- pull(form, str_c("rdrad", rep, sep = "_"))
  lpuln <- pull(form, str_c("lpuln", rep, sep = "_"))
  rpuln <- pull(form, str_c("rpuln", rep, sep = "_"))
  lmuln <- pull(form, str_c("lmuln", rep, sep = "_"))
  rmuln <- pull(form, str_c("rmuln", rep, sep = "_"))
  lduln <- pull(form, str_c("lduln", rep, sep = "_"))
  rduln <- pull(form, str_c("rduln", rep, sep = "_"))
  lothup <- pull(form, str_c("lothup", rep, sep = "_"))
  rothup <- pull(form, str_c("rothup", rep, sep = "_"))
  othupsp <- pull(form, str_c("othupsp", rep, sep = "_"))
  lcerv <- pull(form, str_c("lcerv", rep, sep = "_"))
  rcerv <- pull(form, str_c("rcerv", rep, sep = "_"))
  lthor <- pull(form, str_c("lthor", rep, sep = "_"))
  rthor <- pull(form, str_c("rthor", rep, sep = "_"))
  llumb <- pull(form, str_c("llumb", rep, sep = "_"))
  rlumb <- pull(form, str_c("rlumb", rep, sep = "_"))
  lothspin <- pull(form, str_c("lothspin", rep, sep = "_"))
  rothspin <- pull(form, str_c("rothspin", rep, sep = "_"))
  othspsp <- pull(form, str_c("othspsp", rep, sep = "_"))
  lpfem <- pull(form, str_c("lpfem", rep, sep = "_"))
  rpfem <- pull(form, str_c("rpfem", rep, sep = "_"))
  lmfem <- pull(form, str_c("lmfem", rep, sep = "_"))
  rmfem <- pull(form, str_c("rmfem", rep, sep = "_"))
  ldfem <- pull(form, str_c("ldfem", rep, sep = "_"))
  rdfem <- pull(form, str_c("rdfem", rep, sep = "_"))
  lpat <- pull(form, str_c("lpat", rep, sep = "_"))
  rpat <- pull(form, str_c("rpat", rep, sep = "_"))
  lptib <- pull(form, str_c("lptib", rep, sep = "_"))
  rptib <- pull(form, str_c("rptib", rep, sep = "_"))
  lmtib <- pull(form, str_c("lmtib", rep, sep = "_"))
  rmtib <- pull(form, str_c("rmtib", rep, sep = "_"))
  ldtib <- pull(form, str_c("ldtib", rep, sep = "_"))
  rdtib <- pull(form, str_c("rdtib", rep, sep = "_"))
  lfib <- pull(form, str_c("lfib", rep, sep = "_"))
  rfib <- pull(form, str_c("rfib", rep, sep = "_"))
  lankp <- pull(form, str_c("lankp", rep, sep = "_"))
  rankp <- pull(form, str_c("rankp", rep, sep = "_"))
  lankm <- pull(form, str_c("lankm", rep, sep = "_"))
  rankm <- pull(form, str_c("rankm", rep, sep = "_"))
  ltalus <- pull(form, str_c("ltalus", rep, sep = "_"))
  rtalus <- pull(form, str_c("rtalus", rep, sep = "_"))
  lcalc <- pull(form, str_c("lcalc", rep, sep = "_"))
  rcalc <- pull(form, str_c("rcalc", rep, sep = "_"))
  lfoot <- pull(form, str_c("lfoot", rep, sep = "_"))
  rfoot <- pull(form, str_c("rfoot", rep, sep = "_"))
  lothlo <- pull(form, str_c("lothlo", rep, sep = "_"))
  rothlo <- pull(form, str_c("rothlo", rep, sep = "_"))
  othlosp <- pull(form, str_c("othlosp", rep, sep = "_"))
  lacet <- pull(form, str_c("lacet", rep, sep = "_"))
  racet <- pull(form, str_c("racet", rep, sep = "_"))
  lsacro <- pull(form, str_c("lsacro", rep, sep = "_"))
  rsacro <- pull(form, str_c("rsacro", rep, sep = "_"))
  lsacrum <- pull(form, str_c("lsacrum", rep, sep = "_"))
  rsacrum <- pull(form, str_c("rsacrum", rep, sep = "_"))
  liwing <- pull(form, str_c("liwing", rep, sep = "_"))
  riwing <- pull(form, str_c("riwing", rep, sep = "_"))
  lpsymph <- pull(form, str_c("lpsymph", rep, sep = "_"))
  rpsymph <- pull(form, str_c("rpsymph", rep, sep = "_"))
  lramus <- pull(form, str_c("lramus", rep, sep = "_"))
  rramus <- pull(form, str_c("rramus", rep, sep = "_"))
  lothpelv <- pull(form, str_c("lothpelv", rep, sep = "_"))
  rothpelv <- pull(form, str_c("rothpelv", rep, sep = "_"))
  othplvsp <- pull(form, str_c("othplvsp", rep, sep = "_"))
  injq1 <- pull(form, str_c("injq1", rep, sep = "_"))
  
  
  problems <- form %>% filter(
    ptstatus == 1 & rep <= northinj &
      (is_invalid_na_or_n(fracwith) | fracwith == 0 |
         is_invalid_or_na(openclos) | (fracwith == 1 & openclos == 0) |
         is_invalid_or_na(ggrade) | (openclos == 1 & ggrade == 0) |
         is_invalid_or_na(lclav) | is_invalid_or_na(rclav) | is_invalid_or_na(lscap) | 
         is_invalid_or_na(rscap) | is_invalid_or_na(lphum) | is_invalid_or_na(rphum) | 
         is_invalid_or_na(lmhum) | is_invalid_or_na(rmhum) | is_invalid_or_na(ldhum) | 
         is_invalid_or_na(rdhum) | is_invalid_or_na(lolec) | is_invalid_or_na(rolec) | 
         is_invalid_or_na(lprad) | is_invalid_or_na(rprad) | is_invalid_or_na(lmrad) | 
         is_invalid_or_na(rmrad) | is_invalid_or_na(ldrad) | is_invalid_or_na(rdrad) | 
         is_invalid_or_na(lpuln) | is_invalid_or_na(rpuln) | is_invalid_or_na(lmuln) | 
         is_invalid_or_na(rmuln) | is_invalid_or_na(lduln) | is_invalid_or_na(rduln) | 
         is_invalid_or_na(lothup) | is_invalid_or_na(rothup) | is_invalid(othupsp) | 
         is_invalid_or_na(lcerv) | is_invalid_or_na(rcerv) | is_invalid_or_na(lthor) | 
         is_invalid_or_na(rthor) | is_invalid_or_na(llumb) | is_invalid_or_na(rlumb) | 
         is_invalid_or_na(lothspin) | is_invalid_or_na(rothspin) | is_invalid(othspsp) | 
         is_invalid_or_na(lpfem) | is_invalid_or_na(rpfem) | is_invalid_or_na(lmfem) | 
         is_invalid_or_na(rmfem) | is_invalid_or_na(ldfem) | is_invalid_or_na(rdfem) | 
         is_invalid_or_na(lpat) | is_invalid_or_na(rpat) | is_invalid_or_na(lptib) | 
         is_invalid_or_na(rptib) | is_invalid_or_na(lmtib) | is_invalid_or_na(rmtib) | 
         is_invalid_or_na(ldtib) | is_invalid_or_na(rdtib) | is_invalid_or_na(lfib) | 
         is_invalid_or_na(rfib) | is_invalid_or_na(lankp) | is_invalid_or_na(rankp) | 
         is_invalid_or_na(lankm) | is_invalid_or_na(rankm) | is_invalid_or_na(ltalus) | 
         is_invalid_or_na(rtalus) | is_invalid_or_na(lcalc) | is_invalid_or_na(rcalc) | 
         is_invalid_or_na(lfoot) | is_invalid_or_na(rfoot) | is_invalid_or_na(lothlo) | 
         is_invalid_or_na(rothlo) | is_invalid(othlosp) | is_invalid_or_na(lacet) | 
         is_invalid_or_na(racet) | is_invalid_or_na(lsacro) | is_invalid_or_na(rsacro) | 
         is_invalid_or_na(lsacrum) | is_invalid_or_na(rsacrum) | is_invalid_or_na(liwing) | 
         is_invalid_or_na(riwing) | is_invalid_or_na(lpsymph) | is_invalid_or_na(rpsymph) | 
         is_invalid_or_na(lramus) | is_invalid_or_na(rramus) | is_invalid_or_na(lothpelv) | 
         is_invalid_or_na(rothpelv) | is_invalid(othplvsp) |
         (fracwith== 1 &
            (is_n(lclav) | is_n(rclav) | is_n(lscap) | is_n(rscap) | is_n(lphum) | 
               is_n(rphum) | is_n(lmhum) | is_n(rmhum) | is_n(ldhum) | is_n(rdhum) | 
               is_n(lolec) | is_n(rolec) | is_n(lprad) | is_n(rprad) | is_n(lmrad) | 
               is_n(rmrad) | is_n(ldrad) | is_n(rdrad) | is_n(lpuln) | is_n(rpuln) | 
               is_n(lmuln) | is_n(rmuln) | is_n(lduln) | is_n(rduln) | is_n(lothup) | 
               is_n(rothup) | is_n(othupsp) | is_n(lcerv) | is_n(rcerv) | is_n(lthor) | 
               is_n(rthor) | is_n(llumb) | is_n(rlumb) | is_n(lothspin) | is_n(rothspin) | 
               is_n(othspsp) | is_n(lpfem) | is_n(rpfem) | is_n(lmfem) | is_n(rmfem) | 
               is_n(ldfem) | is_n(rdfem) | is_n(lpat) | is_n(rpat) | is_n(lptib) | 
               is_n(rptib) | is_n(lmtib) | is_n(rmtib) | is_n(ldtib) | is_n(rdtib) | 
               is_n(lfib) | is_n(rfib) | is_n(lankp) | is_n(rankp) | is_n(lankm) | 
               is_n(rankm) | is_n(ltalus) | is_n(rtalus) | is_n(lcalc) | is_n(rcalc) | 
               is_n(lfoot) | is_n(rfoot) | is_n(lothlo) | is_n(rothlo) | is_n(othlosp) | 
               is_n(lacet) | is_n(racet) | is_n(lsacro) | is_n(rsacro) | is_n(lsacrum) | 
               is_n(rsacrum) | is_n(liwing) | is_n(riwing) | is_n(lpsymph) | is_n(rpsymph) | 
               is_n(lramus) | is_n(rramus) | is_n(lothpelv) | is_n(rothpelv) | is_n(othplvsp))) |
         is_invalid_or_n(injq1))) %>%
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