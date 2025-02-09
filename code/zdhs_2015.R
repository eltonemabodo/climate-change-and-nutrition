## Loading all the necessary libraries

library(tidyverse) # main library for data manipulation
library(haven) # for reading SPSS and STATA files
library(readxl) # for reading excel files
library(survey) # for survey analysis
library(srvyr) # for survey analysis
library(here) # for file path management
library(labelled) # for labelled data manipulation
library(sf) # for spatial data manipulation


# Reading the data
zdhs_2015 <- read_dta("data /raw data/zdhs_2015.DTA") %>% 
  # convert labelled data to factors
  to_factor() %>%
  # Select the necessary columns
  select(
    # Survey columns
    caseid, midx, v000, v001, v005, v012, v013, v022, v021, v023, v024, v025, 
    # Maternal and Household characteristics
    v106, v113, v115, v116, v133, v136, v137, v161, v190, v191, v445, v447a, v501,
    v716, v717, v719, v731, v732,
    # Child characteristics
    b2, b4, b5, b8, b11, b19, m14, m15, m18, m19, hw2, hw3, hw4, hw5, hw6, hw7,
    hw8, hw9, hw10, hw11, hw12, hw70, hw71, hw72
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
    wealth_cat = v190,
    wealth_cont = v191,
    bmi = v445,
    maternal_age2 = v447a,
    marital_status = v501,
    maternal_emp = v716,
    maternal_emp1 = v717,
    maternal_emp2 = v719,
    maternal_emp3 = v731,
    maternal_emp4 = v732,
    child_yob = b2,
    child_gender = b4,
    child_alive = b5,
    child_age_yrs = b8,
    birth_int = b11,
    child_age_months = b19,
    antenatal_visits = m14,
    delivery_place = m15,
    birth_size_cat = m18,
    birth_weight_kgs = m19,
    child_weight = hw2,
    child_height = hw3,
    height_age_perc = hw4,
    height_age_sd = hw5,
    height_age_med = hw6,
    weight_age_perc = hw7,
    weight_age_sd = hw8,
    weight_age_med = hw9,
    weight_height_perc = hw10,
    weight_height_sd = hw11,
    weight_height_med = hw12,
    haz = hw70,
    waz = hw71,
    whz = hw72
  ) %>% 
  # Select only children that were alive by the time of the survey
  filter(
    child_alive == "yes"
  ) %>% 
  # Remove all non-dejure respondents
  filter(toilet != "not a dejure resident") %>% 
  mutate(sample_weight = sample_weight/1000000,
         # converting relevant categorical variables to binary variables
         toilet1 = case_when(
           toilet == "flush to piped sewer system" | toilet == "flush to septic tank" |
             toilet == "flush to pit latrine" | toilet == "ventilated improved pit latrine (vip)" |
             toilet == "pit latrine with slab" ~ 1,
           TRUE ~ 0
         ),
         water2 = case_when(
           water == "piped into dwelling" | water == "piped to yard/plot" |
             water == "public tap/standpipe" | water == "tube well or borehole" |
             water == "protected well" | water == "protected spring" |
             water == "rainwater" | water == "bottled water" ~ 1,
           TRUE ~ 0
         ),
         wealth_cat2 = case_when(
           wealth_cat == "poorest" | wealth_cat == "poorer" ~ 1,
           TRUE ~ 0
         ),
         fuel2 = case_when(
           fuel == "electricity" | fuel == "lpg" | fuel == "natural gas" | fuel == "biogas" ~ 1,
           TRUE ~ 0
         ),
         haz = as.numeric(as.character(haz)),
         waz = as.numeric(as.character(waz)),
         whz = as.numeric(as.character(whz)),
         haz1 = ifelse(haz <= -200, 1, 0),
         waz1 = if_else(waz <= -200, 1, 0),
         whz1 = if_else(whz <= -200, 1, 0),
         education_level2 = case_when(
           education_level == "no education" | education_level == "primary" ~ 1,
           TRUE ~ 0
         ),
         agric_employment = case_when(
           maternal_emp1 == "agricultural - self employed" | 
             maternal_emp1 == "agricultural - employee"  ~ 1,
           TRUE ~ 0
         ),
         married = case_when(
           marital_status == "married" | marital_status == "living with partner" ~ 1,
           TRUE ~ 0
         ),
         clinic_delivery = case_when(
           delivery_place != "respondent's home" | delivery_place != "other home" |
             delivery_place != "other" ~ 1,
           TRUE ~ 0
         )
         ) %>% 
  mutate(
    across(c(child_weight, child_height, haz, waz, whz, bmi, water_time,
             birth_weight_kgs, waz1, whz1, haz1, antenatal_visits, birth_int, 
             maternal_age, education_yrs, hh_members, hh_kids, 
             education_level2, wealth_cont, wealth_cat2, fuel2, 
             toilet1, water2, agric_employment, married, clinic_delivery), 
           ~ as.numeric(as.character(.)))
  )

# Load the DHS shapefile
zdhs_geodata_2015 <- st_read("data /raw data/ZWGE72FL/ZWGE72FL.shp")

# Load the district level data

zim_district <- st_read("data /raw data/zwe_adm2_zimstat_ocha/zwe_admbnda_adm2_zimstat_ocha_20180911.shp")

# Use the same crs for both datasets
zim_district <- st_transform(zim_district, crs = st_crs(zdhs_geodata_2015))

# Assign DHS Clusters to Districts - Join DHS Clusters with the districts
zdhs15_district_geodata <- st_join(zdhs_geodata_2015, zim_district, join = st_within)

# Merge the geodata with the zdhs data
zdhs_2015 <- left_join(zdhs_2015, zdhs15_district_geodata, by = c("psu" = "DHSCLUST")) %>% 
  rename(district = ADM2_EN)

# Set the data as survey data for complex survey data analysis
zdhs_2015_survey <- svydesign(
  ids = zdhs_2015$psu,
  data = zdhs_2015,
  strata = zdhs_2015$strata_design,
  weights = zdhs_2015$sample_weight,
  nest = TRUE
)

# Setting the survey for tidyverse-friendly survey design
zdhs_2015_survey <- as_survey_design(zdhs_2015_survey)

# Controlling for scenarios where only one psu will be available
options(survey.lonely.psu = "adjust")

# Create the pseudo-panel for the data
zdhs_2015_pp <- zdhs_2015_survey %>% 
  group_by(district) %>% # To group the data and create the cohorts for the pseudo-panel
  # Results from the code below will be used for analysis
  summarize(
    # Number of children in each cohort
    n = survey_total(),
    # Children related variables
    mean_child_age = survey_mean(child_age_months, na.rm = TRUE),
    mean_birth_int = survey_mean(birth_int, na.rm = TRUE),
    mean_weight = survey_mean(child_weight, na.rm = TRUE),
    mean_height = survey_mean(child_height, na.rm = TRUE),
    mean_haz = survey_mean(haz, na.rm = TRUE),
    mean_waz = survey_mean(waz, na.rm = TRUE),
    mean_whz = survey_mean(whz, na.rm = TRUE),
    mean_birth_weight = survey_mean(birth_weight_kgs, na.rm = TRUE),
    mean_waz1 = survey_mean(waz1, na.rm = TRUE),
    mean_whz1 = survey_mean(whz1, na.rm = TRUE),
    mean_haz1 = survey_mean(haz1, na.rm = TRUE),
    mean_antenatal_visits = survey_mean(antenatal_visits, na.rm = TRUE),
    # Maternal related variables
    mean_maternal_age = survey_mean(maternal_age, na.rm = TRUE),
    mean_education_yrs = survey_mean(education_yrs, na.rm = TRUE),
    mean_bmi = survey_mean(bmi, na.rm = TRUE),
    mean_hh_members = survey_mean(hh_members, na.rm = TRUE),
    mean_water_time = survey_mean(water_time, na.rm = TRUE),
    mean_hh_kids = survey_mean(hh_kids, na.rm = TRUE),
    mean_education_level2 = survey_mean(education_level2, na.rm = TRUE),
    mean_wealth_cont = survey_mean(wealth_cont, na.rm = TRUE),
    mean_wealth_cat2 = survey_mean(wealth_cat2, na.rm = TRUE),
    mean_fuel2 = survey_mean(fuel2, na.rm = TRUE),
    mean_toilet1 = survey_mean(toilet1, na.rm = TRUE),
    mean_water2 = survey_mean(water2, na.rm = TRUE),
    mean_agric_employment = survey_mean(agric_employment, na.rm = TRUE),
    mean_married = survey_mean(married, na.rm = TRUE),
    mean_clinic_delivery = survey_mean(clinic_delivery, na.rm = TRUE)
  ) %>% 
  # Round Numeric values to 2 decimal places
  mutate(across(where(is.numeric), ~ round(., 2))) %>% 
  # Drop all variables ending with _se
  select(-ends_with("_se")) %>% 
  mutate(time = 4) 

# Join the pseudo-panel data with the geodata
zdhs15_pp_geodata <- left_join(zim_district, zdhs_2015_pp, by = c("ADM2_EN"="district"))


# Save the data to the "processed data" folder for merging with other cohorts data from other DHSs
write_dta(zdhs_2015_pp, "data/processed data/processed_pp_2015.dta")



