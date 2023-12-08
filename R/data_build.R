#libraries
library(rio)
library(tidyverse)
library(here)

#### DATA IMPORT ####
load("data/HSLS-school.rdata")
load("data/HSLS-student.rdata")
school_data <- HSLS_09_SCHOOL_v1_0
student_data <- hsls_17_student_pets_sr_v1_0
rm(HSLS_09_SCHOOL_v1_0)
rm(hsls_17_student_pets_sr_v1_0)

#### INITIAL CLEANING ####
# 1. remove suppressed rows
clean_school <- school_data %>% 
  select_if(~!all(str_detect(., "-5"))) %>% 
  select_if(~!all(. == "Data suppressed"))
clean_student <- student_data %>% 
  select_if(~!all(. == "-5")) %>% 
  select_if(~!all(. == "Data suppressed"))

# I ran into issues here, realizing that I can't connect student and school variables
# without access to the restricted dataset. Concentrate on student data.
# outcome of interest = X3TOUTCOME
rm(clean_school)
rm(school_data)
rm(student_data)
#read in codebook
codebook <- import(here("data/HSLS Metadata", "school-vars-codebook.csv"))
vars <- names(clean_student)
#remove restricted vars
subset_vars <- codebook %>% 
  #match to clean_student
  filter(`Variable Name` %in% vars) %>% 
  #drop remaining restricted values
  filter(!`RUF only` == "R") %>% 
  #drop unnecessary cols
  select(!c(`RUF only`, `Both RUF and PUF`)) %>% 
  #keep relevant subsets
  filter(`File Component` == "IDs and weights" |
         `File Component` == "BY student level composites" |
         `File Component` == "F1 student level composites" |
         `File Component` == "HS Transcript student level composites")
vars <- subset_vars$`Variable Name`

#subset clean dataset
dat <- clean_student %>% 
  select(all_of(vars))

#export data for cleaning and modeling
export(dat, "data/hsls-student-subset.csv")
export(subset_vars, "data/HSLS Metadata/subset-prep-plan.csv")

