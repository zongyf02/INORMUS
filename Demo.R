# Run this if you want to restart R
# .rs.restartR()

# Remove all variables in the environment
rm(list = ls())

# Install tidyverse, only needs to run for first time setup
# install.packages("tidyverse")

# Import and tidyverse
library(tidyverse)

# Install devtools, one time setup
# install.packages("devtools")

# Import devtools, only required to update INORMUS
# library(devtools)

# Unimport INORMUS, run before you update the package
# detach("package:INORMUS", unload = TRUE)

# (Re)Install INORMUS package, run this regularly to keep it updated
# install_github("zongyf02/INORMUS")

# Import INORMUS
library(INORMUS)

# Change if needed
setwd("C:\\Users\\Yifan Zong\\Documents\\INORMUSData")

# Read in all forms
form1.1 <- read_form1.1("1.1.csv")
# form1.1_raw <- read_form1.1("1.1.csv", raw = TRUE)

form2.1 <- read_form2.1("2.1.csv")
# form2.1_raw <- read_form2.1("2.1.csv", raw = TRUE)

form2.2 <- read_form2.2("2.2.csv")
# form2.2_raw <- read_form2.2("2.2.csv", raw = TRUE)

form3.1 <- read_form3.1("3.1.csv")
# form3.1_raw <- read_form3.1("3.1.csv", raw = TRUE)

form3.2 <- read_form3.2("3.2.csv")
# form3.2_raw <- read_form3.2("3.2.csv", raw = TRUE)

form4.1 <- read_form4.1("4.1.csv")
# form4.1_raw <- read_form4.1("4.1.csv", raw = TRUE)

form5.1 <- read_form5.1("5.1.csv")
# form5.1_raw <- read_form5.1("5.1.csv", raw = TRUE)

form5.2 <- read_form5.2("5.2.csv")
# form5.2_raw <- read_form5.2("5.2.csv", raw = TRUE)

form5.3 <- read_form5.3("5.3.csv")
# form5.3_raw <- read_form5.3("5.3.csv", raw = TRUE)

form5.4 <- read_form5.4("5.4.csv")
# form5.4_raw <- read_form5.4("5.4.csv", raw = TRUE)

form5.5 <- read_form5.5("5.5.csv")
# form5.5_raw <- read_form5.5("5.5.csv", raw = TRUE)

form5.6 <- read_form5.6("5.6.csv")
# form5.6_raw <- read_form5.6("5.6.csv", raw = TRUE)

form5.7 <- read_form5.7("5.7.csv")
# form5.7_raw <- read_form5.7("5.7.csv", raw = TRUE)

form5.8 <- read_form5.8("5.8.csv")
# form5.8_raw <- read_form5.8("5.8.csv", raw = TRUE)

form5.9 <- read_form5.9("5.9.csv")
# form5.9_raw <- read_form5.9("5.9.csv", raw = TRUE)

form5.10 <- read_form5.10("5.10.csv")
# form5.10_raw <- read_form5.10("5.10.csv", raw = TRUE)

form5.11 <- read_form5.11("5.11.csv")
# form5.11_raw <- read_form5.11("5.11.csv", raw = TRUE)

form5.12 <- read_form5.12("5.12.csv")
# form5.12_raw <- read_form5.12("5.12.csv", raw = TRUE)

form5.13 <- read_form5.13("5.13.csv")
# form5.13_raw <- read_form5.13("5.13.csv", raw = TRUE)

form5.14 <- read_form5.14("5.14.csv")
# form5.14_raw <- read_form5.14("5.14.csv", raw = TRUE)

form6.1 <- read_form6.1("6.1.csv")
# form6.1_raw <- read_form6.1("6.1.csv", raw = TRUE)

form7.1 <- read_form7.1("7.1.csv")
# form7.1_raw <- read_form7.1("7.1.csv", raw = TRUE)

form7.2 <- read_form7.2("7.2.csv")
# form7.2_raw <- read_form7.2("7.2.csv", raw = TRUE)

form7.3 <- read_form7.3("7.3.csv")
# form7.3_raw <- read_form7.3("7.3.csv", raw = TRUE)

form7.4 <- read_form7.4("7.4.csv")
# form7.4_raw <- read_form7.4("7.4.csv", raw = TRUE)

# Create a list of all forms
forms <- list(form1.1, form2.1, form2.2, form3.1, form3.2, form4.1, form5.1,
              form5.2, form5.3, form5.4, form5.5, form5.6, form5.7, form5.8,
              form5.9, form5.10, form5.11, form5.12, form5.13, form5.14, form6.1,
              form7.1, form7.2, form7.3, form7.4)

# Merge all forms
form <- merge_forms(forms)

# View the structure of all columns of form
str(form, list.len = ncol(form))

################################################################################

# Create a table with region, site, studyid, and sex columns
select(form, c(region, site, studyid, sex))

# Create a table with columns starting with "pneu"
select(form, starts_with("pneu"))

################################################################################

# Create a table with only male patients
filter(form, sex == 1)

# Create a table with only admitted patients, grouped by sex
filter(form, ptstatus == 1) %>%
  group_by(sex)

################################################################################

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

################################################################################

# Create a summary table of number of patients in each region
form %>% group_by(region) %>%
  summarize(n = n())
# equivalently use the library function
summarize_form(form, "studyid", region)

# Create a summary table of number of admitted patients in each region
form %>% group_by(region) %>%
  summarize(nAdmitted = sum(ptstatus == 1, na.rm = TRUE))
# equivalently, use the library function
summarize_form(form, "ptstatus", region)

# Create a summary table of patient consent date grouped by region and site
form %>% group_by(region, site) %>%
  summarize(minDate = min(parse_dmY(condate), na.rm = TRUE),
            maxDate = max(parse_dmY(condate), na.rm = TRUE))
# equivalently
summarize_form(form, "condate", region, site)

# Create a summary table of patient age grouped by region and site
form %>% group_by(region, site) %>%
  summarize(minAge = min(age, na.rm = TRUE),
            maxAge = max(age, na.rm = TRUE),
            meanAge = mean(age, na.rm = TRUE),
            sdAge = sd(age, na.rm = TRUE))
# equivalently
summarize_form(form, "age", region, site)

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

# Create summary table of the first 11 columns grouped by region
# Note the first two omitted. The function will throw an error if asked
# to summarize a column used for grouping
summarize_form(form, colnames(form)[3:11], region)

################################################################################
# Check coding boxes
problems_form2.1_box5 <- check_form2.1_box5(form)
problems_form2.2_box10 <- check_form2.2_box10(form)
problems_form3.1_box4 <- check_form3.1_box4(form)
problems_form3.2_box5 <- check_form3.2_box5(form)
problems_form3.2_box6 <- check_form3.2_box6(form)
problems_form3.2_box7_chest <- check_form3.2_box7_chest(form)
problems_form3.2_box7_abdo <- check_form3.2_box7_abdo(form)
problems_form3.2_box7_hn <- check_form3.2_box7_hn(form)
problems_form4.1_box2 <- check_form4.1_box2(form)
problems_form4.1_box3 <- check_form4.1_box3(form)
problems_form4.1_box5 <- check_form4.1_box5(form)

# Checking date consistencies
problems_condate_injdate <- check_condate_injdate(form)
problems_hspdate_injdate <- check_hspdate_injdate(form)
problems_condate_hspdate <- check_condate_hspdate(form)
problems_injdate_hspdate <- check_injdate_hspdate(form)

# Check number of injuries
problem_northinf <- check_northinj(form)
