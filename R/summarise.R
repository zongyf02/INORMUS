#' Same as summarise_form
#'
#' #' @param form any form
#' @param cols names of the columns to summarise
#' @param ... names of the columns to group by
#' @return summary dataframe
#' @import tidyverse
#' @export
#' @export
summarize_form <- function(form, cols, ...) {
  summarise_form(form, cols, ...)
}

#' Summarise a form
#'
#' Summarise the given columns of a form
#' Note that cols and ... cannot contain the same columns
#'
#' @param form any form
#' @param cols names of the columns to summarise
#' @param ... names of the columns to group by
#' @return summary dataframe
#' @import tidyverse
#' @export
summarise_form <- function(form, cols, ...) {
  if(typeof(cols) != "character") {
    stop("Unrecognized column names", cols)
  }

  first <- TRUE

  if (is.null(substitute(...))) {
    grouped_form <- form %>% mutate(Total = "") %>% group_by(Total)
  } else {
    grouped_form <- form %>% group_by(...)
  }

  for (i in 1:length(cols)) {
    if (cols[i] == "studyid") {
      # if column is studyid
      if (first) {
        res <- summarise(grouped_form, n = n())
        first <- FALSE
      } else {
        res <- left_join(res, summarise(grouped_form, n = n()))
      }
    }else if (regexpr("(date)", cols[i]) != -1) {
      # if column contains date
      if (first) {
        res <- summarise_date(grouped_form, cols[i])
        first <- FALSE
      } else {
        res <- left_join(res, summarise_date(grouped_form, cols[i]))
      }
    } else if ("age" == cols[i] || regexpr("(days|hrs)", cols[i]) != -1) {
      # if column contains a continuous variable
      if (first) {
        res <- summarise_cont(grouped_form, cols[i])
        first <- FALSE
      } else {
        res <- left_join(res, summarise_cont(grouped_form, cols[i]))
      }
    } else if (!is.character(pull(form, cols[i]))) {
      # else column contains a categorical variable
      vals <- sort(unique(select(form, all_of(cols[i]))[[1]]))
      if (first) {
        res <- summarise_cat(grouped_form, cols[i], vals)
        first <- FALSE
      } else {
        res <- left_join(res, summarise_cat(grouped_form, cols[i], vals))
      }
    }
  }

  if (first) {
    return(NA)
  } else {
    return(res)
  }
}

#' Summarise a date column
#'
#' Summarise min, max of dates in that column
#'
#' @param grouped_form any grouped dataframe
#' @param colname name to column to summarise
#' @return summary dataframe
#' @import tidyverse
summarise_date <- function(grouped_form, colname) {
  res <- grouped_form %>%
    summarise(
      across(colname,
             list("min" = function(x){min(parse_dmY(x), na.rm = TRUE)},
                  "max" = function(x){max(parse_dmY(x), na.rm = TRUE)}),
             .names = "{.col}_{.fn}"))

  return (left_join(res, summarise_NA(grouped_form, colname)))
}

#' Summarise a continuous column
#'
#' Summarise min, max, mean, and sd of values in that column
#'
#' @param grouped_form any grouped dataframe
#' @param colname name to column to summarise
#' @return summary dataframe
#' @import tidyverse
summarise_cont <- function(grouped_form, colname) {
  res <- grouped_form %>%
    summarise(
      across(colname,
             list("min" = function(x){min(x, na.rm = TRUE)},
                  "max" = function(x){max(x, na.rm = TRUE)},
                  "mean"  = function(x){mean(x, na.rm = TRUE)},
                  "sd" = function(x){sd(x, na.rm = TRUE)}),
             .names = "{.col}_{.fn}"))

  return (left_join(res, summarise_NA(grouped_form, colname)))
}

#' Summarise a categorical column
#'
#' Summarise the number and percentage of values in a column
#'
#' @param grouped_form any grouped dataframe
#' @param colname name to column to summarise
#' @param vals values in that column
#' @return summary dataframe
#' @import tidyverse
summarise_cat <- function(grouped_form, colname, vals) {
  for (i in 1:length(vals)) {
    if (i == 1) {
      res <- summarise_catval(grouped_form, colname, vals[i])
    } else {
      res <- left_join(res, summarise_catval(grouped_form, colname, vals[i]))
    }
  }

  if (length(vals)) {
    res <- left_join(res, summarise_NA(grouped_form, colname))
  } else {
    res <- summarise_NA(grouped_form, colname)
  }

  return(res)
}

#' Summarise a value in a column
#'
#' Summarise the number and percentage of that value in a column
#'
#' @param grouped_form any grouped dataframe
#' @param colname name to column to summarise
#' @return summary dataframe
#' @import tidyverse
summarise_catval <- function(grouped_form, colname, val) {
  grouped_form %>%
    summarise(
      across(colname,
             list("n" = function(x){sum(x == val, na.rm = TRUE)},
                  "p" = function(x){percent(mean(x == val, na.rm = TRUE))}),
             .names = paste("{.col}_{.fn}", val, sep = "")))
}

#' Summarise NAs in a column
#'
#' Summarise the number and percentage of NA in  acolumn
#'
#' @param grouped_form any grouped dataframe
#' @param colname name to column to summarise
#' @return summary dataframe
#' @import tidyverse
summarise_NA <- function(grouped_form, colname) {
  grouped_form %>%
    summarise(
      across(colname,
             list("n" = function(x){sum(is.na(x))},
                  "p" = function(x){percent(mean(is.na(x)))}),
             .names = "{.col}_{.fn}NA"))
}
