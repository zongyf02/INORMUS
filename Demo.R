library(tidyverse)
# library(devtools)
# install_github("zongyf02/INORMUS")
library(INORMUS)

form1.1 <- read_form1.1("1.1.csv")
form1.1_raw <- read_form1.1("1.1.csv", raw = TRUE)

form2.1 <- read_form2.1("2.1.csv")
form2.1_raw <- read_form2.1("2.1.csv", raw = TRUE)

form2.2 <- read_form2.2("2.2.csv")
form2.2_raw <- read_form2.2("2.2.csv", raw = TRUE)

form3.1 <- read_form3.1("3.1.csv")
form3.1_raw <- read_form3.1("3.1.csv", raw = TRUE)

form3.2 <- read_form3.2("3.2.csv")
form3.2_raw <- read_form3.2("3.2.csv", raw = TRUE)

form4.1 <- read_form4.1("4.1.csv")
form4.1_raw <- read_form4.1("4.1.csv", raw = TRUE)

form5.1 <- read_form5.1("5.1.csv")
form5.1_raw <- read_form5.1("5.1.csv", raw = TRUE)

form5.2 <- read_form5.2("5.2.csv")
form5.2_raw <- read_form5.2("5.2.csv", raw = TRUE)

form5.3 <- read_form5.3("5.3.csv")
form5.3_raw <- read_form5.3("5.3.csv", raw = TRUE)

form5.4 <- read_form5.4("5.4.csv")
form5.4_raw <- read_form5.4("5.4.csv", raw = TRUE)

form5.5 <- read_form5.5("5.5.csv")
form5.5_raw <- read_form5.5("5.5.csv", raw = TRUE)

form5.6 <- read_form5.6("5.6.csv")
form5.6_raw <- read_form5.6("5.6.csv", raw = TRUE)

form5.7 <- read_form5.7("5.7.csv")
form5.7_raw <- read_form5.7("5.7.csv", raw = TRUE)

form5.8 <- read_form5.8("5.8.csv")
form5.8_raw <- read_form5.8("5.8.csv", raw = TRUE)

form5.9 <- read_form5.9("5.9.csv")
form5.9_raw <- read_form5.9("5.9.csv", raw = TRUE)

form5.10 <- read_form5.10("5.10.csv")
form5.10_raw <- read_form5.10("5.10.csv", raw = TRUE)

form5.11 <- read_form5.11("5.11.csv")
form5.11_raw <- read_form5.11("5.11.csv", raw = TRUE)

form5.12 <- read_form5.12("5.12.csv")
form5.12_raw <- read_form5.12("5.12.csv", raw = TRUE)

form5.13 <- read_form5.13("5.13.csv")
form5.13_raw <- read_form5.13("5.13.csv", raw = TRUE)

form5.14 <- read_form5.14("5.14.csv")
form5.14_raw <- read_form5.14("5.14.csv", raw = TRUE)

form6.1 <- read_form6.1("6.1.csv")
form6.1_raw <- read_form6.1("6.1.csv", raw = TRUE)

form7.1 <- read_form7.1("7.1.csv")
form7.1_raw <- read_form7.1("7.1.csv", raw = TRUE)

form7.2 <- read_form7.2("7.2.csv")
form7.2_raw <- read_form7.2("7.2.csv", raw = TRUE)

form7.3 <- read_form7.3("7.3.csv")
form7.3_raw <- read_form7.3("7.3.csv", raw = TRUE)

form7.4 <- read_form7.4("7.4.csv")
form7.4_raw <- read_form7.4("7.4.csv", raw = TRUE)

forms <- list(form1.1, form2.1, form2.2, form3.1, form3.2, form4.1, form5.1,
              form5.2, form5.3, form5.4, form5.5, form5.6, form5.7, form5.8,
              form5.9, form5.10, form5.11, form5.12, form5.13, form5.14, form6.1,
              form7.1, form7.2, form7.3, form7.4)

form <- merge(forms)

rm(list = ls())
.rs.restartR()
