# Demo

## Getting started
<br>

> The following demo is provided with the assumption that **RStudio** has already been installed. If not, please download from [RStudio.com](https://www.rstudio.com/) first.


### _Setting up the R environment_
```R
# To restart R
.rs.restartR()

# Remove all variables in the environment
rm(list = ls())

# for first time setup
install.packages("tidyverse")

# import tidyverse
library(tidyverse)

# Install devtools, one time setup
install.packages("devtools")

#Import devtools, only required to update INORMUS
library(devtools)

# set working directory (change if needed)
setwd("path_to_current_working_folder")
```

### _To use INORMUS_ 
```R
# Unimport INORMUS, run before you update the package
detach("package:INORMUS", unload = TRUE)

# (Re)Install INORMUS package, run this regularly to keep it updated
install_github("zongyf02/INORMUS")

# Import INORMUS
library(INORMUS)
```

If you would like to try out our demo locally, you can either clone our repo or copy-paste the R script from the following link:

<a href="./Demo.R" download>Demo.R</a>

## Usage

### _To read forms_
> Data can be either read raw (as is) or formatted by calling the function **_read_formX.Y("path", raw)_** with raw set to true or false respectively. By default raw is **false**.
```R
# read form1.1 (formatted)
form1.1 <- read_form1.1("1.1.csv")

# read form1.1 (raw)
form1.1 <- read_form1.1("1.1.csv", raw = TRUE)
```
<hr>

### _To merge forms_

>When manipulating data, it is important to be able to merge different tables (forms) together. Hence we have provided the **_merge_forms_** function for this purpose.
```R
# Create a list of all forms
forms <- list(form1.1, form2.1, form2.2, form3.1,
              form3.2, form4.1, form5.1, form5.2, 
              form5.3, form5.4, form5.5, form5.6, 
              form5.7, form5.8, form5.9, form5.10, 
              form5.11, form5.12, form5.13, form5.14, 
              form6.1, form7.1, form7.2, form7.3, form7.4)

# Merge all forms
form <- merge_forms(forms)

# View the structure of all columns of form
str(form, list.len = ncol(form))
```
<hr>

### _Creating tables from forms_
 > Sometimes, we don't need to see the entire form as it can be cumbersome to work with and will contain information that are irrelevant to us. As such , we will demonstrate below how you can create a better, more workable table from a form.  

1.&nbsp; **Working with columns**
   
* Selecting **_specific columns_** from a form
```R
# Create a table with region, site, studyid, and sex columns
select(form, c(region, site, studyid, sex))
```

* Selecting columns from a form based on **_keywords_**
```R
# Create a table with columns starting with "pneu"
select(form, starts_with("pneu"))
```
2.&nbsp; **Working with rows** 
> filter goes through the form **_row by row_** and outputs a table with all the rows that match the filter conditions.
```R
# Create a table with only male patients
filter(form, sex == 1)

# Create a table with only admitted patients, grouped by sex
filter(form, ptstatus == 1) %>% group_by(sex)
```
<hr>

### _Modifying tables_
> We use mutate to add new variables(columns) to the table while preserving current ones. 

```R
# Replace 1, 0 in ptstatus by "Admitted", "Not Admitted"
mutate(form, ptstatus = if_else(ptstatus == 1, "Admitted", "Not Admitted"))

# Compress the range of the income column
mutate(form, income = case_when(income == 0 ~ 0,
                                income == 1 | income == 2 ~ 1,
                                income == 3 | income == 4 ~ 2,
                                income == 5 | income == 6 ~ 3,
                                income == 7 | income == 8 ~ 4,
                                income == 9 ~ 5,
                                income == 10 ~ 6,
                                income == 11 ~ 7))
```
<hr>

### _Summarizing Functions_
1.&nbsp; Summary of number of patients by region

```R
# Create a summary table of number of patients in each region
form %>% group_by(region) %>% summarize(n = n())

# equivalently use the library function
summarize_form(form, "studyid", region)
```

2.&nbsp; Summary of patient consent date by region and site

```R
# Create a summary table of patient consent date grouped by region and site
form %>% group_by(region, site) %>%
  summarize(minDate = min(parse_dmY(condate), na.rm = TRUE),
            maxDate = max(parse_dmY(condate), na.rm = TRUE))
# equivalently
summarize_form(form, "condate", region, site)
```
3.&nbsp; Summary of patient age by region and site
```R
# Create a summary table of patient age grouped by region and site
form %>% group_by(region, site) %>%
  summarize(minAge = min(age, na.rm = TRUE),
            maxAge = max(age, na.rm = TRUE),
            meanAge = mean(age, na.rm = TRUE),
            sdAge = sd(age, na.rm = TRUE))
# equivalently
summarize_form(form, "age", region, site)
```

4.&nbsp; Summary of admitted patients' sex by region and site
```R
# Create a summary table of admitted patients' sex grouped by region and site
form %>% group_by(region, site) %>% filter(ptstatus == 1) %>%
  summarize(male = sum(sex == 1, na.rm = TRUE),
            percentMale = mean(sex == 1, na.rm = TRUE),
            female = sum(sex ==2, na.rm = TRUE),
            percentFemale = mean(sex ==2, na.rm = TRUE),
            other = sum(sex != 1 && sex != 2),
            percentOther = mean(sex != 1 && sex != 2))
#equivalently
summarize_form(filter(form, ptstatus == 1), "sex", region, site)
```
5.&nbsp; Summary of first 11 columns by region
> Note: first two columns are omitted. Function will throw error if asked to summarize a column used for grouping.
```R 
# Create summary table of the first 11 columns grouped by region
summarize_form(form, colnames(form)[3:11], region)
```
<hr>

### _Data Validation_
> As with any study, errors and accidental omissions of data can occur. Thus, we have to clean the data and filter out invalid cases. Below you will find examples of custom filter functions we used to clean the data.

1.&nbsp; Coding box checks
```R
# IE coding box related problems in question 5 form2.1
problems_form2.1_box5 <- check_form2.1_box5(form)

# IE coding box related problems in question 1 form5.1x (rep is the number of iterations)
problems_form5.1_box1 <- check_form5.1x_box1(form, rep)
```

2.&nbsp; Numbered Checks
```R
# Check that consent date should be on the same day, or after the date of injury
# Check 1
problems_condate_injdate <- check_condate_injdate(form)

# Check that hospital admission date should be on the same day, or after the date of injury
# Check 2
problems_hspdate_injdate <- check_hspdate_injdate(form)

# Check that consent date should be 0 - 30 days after hospital admission date
# Check 3
problems_condate_hspdate <- check_condate_hspdate(form)

# Check that the number of orthopedic injuries stated on form 3.2 is consistent with the number of sets of injury forms completed
# Check 4
problems_northinf <- check_northinj(form)
#' Check that the number of orthopedic injuries stated on form 3.2 is consistent with the Wound & Skin Prep form 5.14
problems_northinj_form5.14 <- check_northinj_form5.14(form)

# Check that time from injury to hsp admission should be within +/- 24 hrs range of difference between injdate and hspdate
# Check 5
problems_injdate_hspdate <- check_injdate_hspdate(form)

# The time from injury to hospital admission should be within 24 hours if the patient is coming from the Accident/Injury Site
# Check 6
problems_admfrom_ihunits <- check_admfrom_ihunits(form)

# Check that for antibiotics first administered at the injury scene, time from injury to antibiotics administration is at most 5 hours before time from injury to hospital admission
# Check 7
problems_abx_locabx_1 <- check_abx_locabx_1(form)

# Check that the location of fracture and the location of dislocation in one set of form5.x are related
# Also ensures only 1 fracture per set of form
# Checks 8 and 9
problems_fracwith_diswith_1 <- check_fracwith_diswith(form, 1)
problems_fracwith_diswith_2 <- check_fracwith_diswith(form, 2)
problems_fracwith_diswith_3 <- check_fracwith_diswith(form, 3)

# Check that the response to I&D is consistent with whether the fracture is open or closed 
# Check 11
problems_openclos_iandd_1 <- check_openclos_iandd(form, 1)
problems_openclos_iandd_2 <- check_openclos_iandd(form, 2)
problems_openclos_iandd_3 <- check_openclos_iandd(form, 3)

# Check that details on the patient's surgery is consistent
# Check 12
problems_operat_failsurg_delsurg_1 <- check_operat_failsurg_delsurg(form, 1)
problems_operat_failsurg_delsurg_2 <- check_operat_failsurg_delsurg(form, 2)
problems_operat_failsurg_delsurg_3 <- check_operat_failsurg_delsurg(form, 3)

# Check that closed fracture injuries have have NA selected in form5.14
# Check 13
problems_openclos_NA <- check_openclos_NA(form)
```

3.&nbsp; Invalid/Missing Entries checks
```R
# Check for invalid/missing entries in form1.1
problems_invalid_form1.1 <- check_invalid_form1.1(form)
# Check for invalid/missing entries in form2.1
problems_invalid_form2.1 <- check_invalid_form2.1(form)
# Check for invalid/missing entries in form2.2
problems_invalid_form2.2 <- check_invalid_form2.2(form)
# Check for invalid/missing entries in form3.1, do not check question 4.1, intent of injury
problems_invalid_form3.1 <- check_invalid_form3.1(form, checkIntent = FALSE)
# Check for invalid/missing entries in form3.2
problems_invalid_form3.2 <- check_invalid_form3.2(form)
# Check for invalid/missing entries in form4.1
problems_invalid_form4.1 <- check_invalid_form4.1(form)
# Check for invalid/missing entries in form5.2
problems_invalid_form5.2x <- check_invalid_form5.2x(form, 1)
problems_invalid_form5.2x <- check_invalid_form5.2x(form, 2)
problems_invalid_form5.2x <- check_invalid_form5.2x(form, 3)
```



