## Loading all the necessary libraries

library(tidyverse) # main library for data manipulation
library(haven) # for reading SPSS and STATA files
library(readxl) # for reading excel files
library(survey) # for survey analysis
library(srvyr) # for survey analysis


# Reading the data
zdhs_2015 <- read_dta("climate-change-and-nutrition/data /raw data/zdhs_2015.DTA") %>% 
  # Select the necessary columns
  select() %>% # To add the necessary columns
  # Rename the columns
  rename() %>% # To rename the columns
  # Set variable labels
  set_variable_labels() # To set the variable labels


# Set the data as survey data
zdhs_2015_survey <- as_survey(zdhs_2015, weight = wt) # To set the data as survey data

# Create the pseudo-panel for the data
zdhs_2015_pp <- zdhs_2015_survey %>% 
  group_by(Province, ChildGender) %>% # To group the data and create the cohorts for the pseudo-panel
  summarise() # To summarise the data. This is the data to be merged with other pseudo-panels from other dhss


# Save the data to the "processed data" folder for merging with other cohorts data from other DHSs

write_dta()





