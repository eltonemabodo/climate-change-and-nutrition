# Loading all the necessary libraries


library(tidyverse) # main library for data manipulation
library(haven) # for reading SPSS and STATA files
library(readxl) # for reading excel files
library(survey) # for survey analysis
library(srvyr) # for survey analysis
library(here) # for file path management
library(labelled) # for labelled data manipulation

# loading the data

zdhs_1999 <- read_dta("climate-change-and-nutrition/data /raw data/zdhs_1999.DTA") %>% 
  # convert labelled data to factors
  to_factor() %>%
  # Select the necessary columns
  select(
    # Survey columns
    caseid, midx, v000, v001, v005, v012, v013, v022, v021, v023, v024, v025, 
    # Maternal and Household characteristics
    v106, v113, v115, v116, v133, v136, v137, v161, v445, v447a, v501,
    v716, v717, v719, v731, v732,
    # Child characteristics
    b2, b4, b5, b8, b11, hw1, m14, m15, m18, m19, hw2, hw3, hw4, hw5, hw6, hw7, # hw1 replaced b19 for child age in months
    hw8, hw9, hw10, hw11, hw12
    ) %>% 
  # Rename the columns
  rename(
    code_phase = v000, 
    cluster_num = v001,
    sample_weight = v005,
    maternal_age = v012,
    maternal_age_grp = v013,
    psu = v021,
    strata_errors = v022,
    strata_design = v023,
    province = v024,
    residence = v025,
    education_level = v106,
    water = v113,
    water_time = v115,
    toilet = v116,
    education_yrs = v133,
    hh_members = v136,
    hh_kids = v137,
    fuel = v161,
    bmi = v445,
    maternal_age2 = v447a,
    marital_status = v501,
    maternal_emp = v716,
    maternal_emp1 = v717,
    maternal_emp2 = v719,
    maternal_emp3 = v731,
    maternal_emp4 = v732,
    child_yob = b2,
    child_age_months = hw1, # child age in months