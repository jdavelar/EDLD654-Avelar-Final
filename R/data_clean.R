#### PREP DATASET WITH {RECIPES} ####
library(tidyverse)
library(here)
library(rio)
library(recipes)
dat <- import(here("data", "hsls-student-subset.csv"))

#need to do some initial cleaning first
#transform values to missing
dat[dat == "Missing"] <- NA
dat[dat == -8] <- NA
dat[dat == -6] <- NA
dat[dat == -9] <- NA
dat[dat == "Unit non-response"] <- NA
dat[dat == "Item legitimate skip/NA"] <- NA
dat[dat == "Component not applicable"] <- NA
dat[dat == "Nonrespondent"] <- NA

#transform yes/no binaries
convert_binary <- function(data) {
  data <- mutate_all(data, ~ ifelse(. == "Yes", 1, ifelse(. == "No", 0, .)))
  return(data)
}
dat <- convert_binary(dat)

#fix other vars - custom case_when not working; use manual
dat <- dat %>% 
  #removed original composite vars that were already dummy coded
  #removed duplicate demo vars for student (gender/race)
  select(!c(X2UNIV1, X3UNIV1, X4UNIV1, X2SEX, X2HISPANIC, X2WHITE, X2RACE, X2BLACK, X2DUALLANG)) %>% 
  #outlier binary vars to 0/1 format
  mutate(
  X1WHITE = case_when(
    X1WHITE == "Student is White" ~ 1,
    X1WHITE == "Student is not White" ~ 0
  ),
  X1BLACK = case_when(
    X1BLACK == "Student is Black" ~ 1,
    X1BLACK == "Student is not Black" ~ 0
  ),
  X2UNIV2A = case_when(
    X2UNIV2A == "Base year respondent" ~ 1,
    X2UNIV2A == "Base year nonparticipant" ~ 0
  ),
  X1SEX = case_when(
    X1SEX == "Male" ~ 0,
    X1SEX == "Female" ~ 1
  ),
  X1PARRESP = case_when(
    X1PARRESP == "Parent questionnaire respondent is P1" ~ 1,
    X1PARRESP == "Parent questionnaire respondent is neither P1 nor P2" ~ 0
  ),
  X1PAR1OCC_STEM1 = case_when(
    X1PAR1OCC_STEM1 == "Life and Physical Science, Engineering, Mathematics, and Information Technology Occupations" ~ 1,
    (!is.na(X1PAR1OCC_STEM1) & X1PAR1OCC_STEM1 != 1) ~ 0
  ),
  X1PAR2OCC_STEM1 = case_when(
    X1PAR2OCC_STEM1 == "Life and Physical Science, Engineering, Mathematics, and Information Technology Occupations" ~ 1,
    (!is.na(X1PAR2OCC_STEM1) & X1PAR2OCC_STEM1 != 1) ~ 0
  ),
  X1MOMRESP = case_when(
    X1MOMRESP == "Parent respondent is not bio/adoptive/step-mother" ~ 1,
    X1MOMRESP == "Parent respondent is bio/adoptive/step-mother" ~ 0
  ),
  X1DADRESP = case_when(
    X1DADRESP == "Parent respondent is bio/adoptive/step-father" ~ 1,
    X1DADRESP == "Parent respondent is not bio/adoptive/step-father" ~ 0
  ),
  X1POVERTY = case_when(
    X1POVERTY == "At or above poverty threshold" ~ 0,
    X1POVERTY == "Below poverty threshold" ~ 1
  ),
  X1POVERTY130 = case_when(
    X1POVERTY130 == "At or above 130% poverty threshold" ~ 0,
    X1POVERTY130 == "Below 130% poverty threshold" ~ 0
  ),
  X1POVERTY185 = case_when(
    X1POVERTY185 == "At or above 185% poverty threshold" ~ 0,
    X1POVERTY185 == "Below 185% poverty threshold" ~ 0
  ),
  X1STU30OCC_STEM1 = case_when(
    X1STU30OCC_STEM1 == "Life and Physical Science, Engineering, Mathematics, and Information Technology Occupations" ~ 1,
    (!is.na(X1STU30OCC_STEM1) & X1STU30OCC_STEM1 != 1) ~ 0
  ),
  X1IEPFLAG = case_when(
    X1IEPFLAG == "Student has no IEP" ~ 0,
    X1IEPFLAG == "Student has an IEP" ~ 1
  ),
  X1PQSTAT = case_when(
    X1PQSTAT == "Respondent" ~ 1,
    TRUE ~ 0
  ),
  X1PQLANG = case_when(
    X1PQLANG == "English interview" ~ 0,
    X1PQLANG == "Spanish interview" ~ 1
  ),
  X1TSQSTAT = case_when(
    X1TSQSTAT == "Respondent" ~ 1,
    X1TSQSTAT == "Student has no fall 2009 sci class (survey component not applicable)" ~ 0
  ),
  X1CONTROL = case_when(
    X1CONTROL == "Public" ~ 1,
    TRUE ~ 0
  ),
  X2PAR1OCC_STEM1 = case_when(
    X2PAR1OCC_STEM1 == "Life and Physical Science, Engineering, Mathematics, and Information Technology Occupations" ~ 1,
    (!is.na(X2PAR1OCC_STEM1) & X2PAR1OCC_STEM1 != 1) ~ 0
  ),
  X2PAR2OCC_STEM1 = case_when(
    X2PAR2OCC_STEM1 == "Life and Physical Science, Engineering, Mathematics, and Information Technology Occupations" ~ 1,
    (!is.na(X2PAR2OCC_STEM1) & X2PAR2OCC_STEM1 != 1) ~ 0
  ),
  X2DADRESP = case_when(
    X2DADRESP == "Parent respondent is bio/adoptive/step-father" ~ 1,
    X2DADRESP == "Parent respondent is not bio/adoptive/step-father" ~ 0
  ),
  X2POVERTY = case_when(
    X2POVERTY == "At or above poverty threshold" ~ 0,
    X2POVERTY == "Below poverty threshold" ~ 1
  ),
  X2POVERTY130 = case_when(
    X2POVERTY130 == "At or above 130% poverty threshold" ~ 0,
    X2POVERTY130 == "Below 130% poverty threshold" ~ 0
  ),
  X2POVERTY185 = case_when(
    X2POVERTY185 == "At or above 185% poverty threshold" ~ 0,
    X2POVERTY185 == "Below 185% poverty threshold" ~ 0
  ),
  X2STU30OCC_STEM1 = case_when(
    X2STU30OCC_STEM1 == "Life and Physical Science, Engineering, Mathematics, and Information Technology Occupations" ~ 1,
    (!is.na(X2STU30OCC_STEM1) & X2STU30OCC_STEM1 != 1) ~ 0
  ),
  X2PQLANG = case_when(
    X2PQLANG == "English interview" ~ 0,
    X2PQLANG == "Spanish interview" ~ 1
  ),
  X2CONTROL = case_when(
    X2CONTROL == "Public" ~ 1,
    X2CONTROL == "Catholic or other private" ~ 0
  ),
  X3ELLSTATUS = case_when(
    X3ELLSTATUS == "Not English as second language" ~ 0,
    X3ELLSTATUS == "English as a second language" ~ 1
  )) %>% 
    #categorical vars to numeric if necessary
    mutate(X1HHNUMBER = as.numeric(str_remove_all(X1HHNUMBER, "\\D")
    ),
    X2NUMHS = as.numeric(str_remove_all(X2NUMHS, "\\D")),
    X2HHNUMBER = as.numeric(str_remove_all(X2HHNUMBER, "\\D")
    ),
    X3TTRNRCVD = case_when(
      X3TTRNRCVD == "One" ~ 1,
      X3TTRNRCVD == "Two" ~ 2,
      X3TTRNRCVD == "Three" ~ 3
    ))

#save clean data
export(dat, "data/hsls-student-clean.csv")
