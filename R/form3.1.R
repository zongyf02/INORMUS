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
     mutate(injdate = parse_dmY(injdate),
            region = parse_region(studyid),
            site = parse_site(studyid)) %>%
     relocate(c(region, site), .before = studyid)
 }

  return(form)
}
