library(tidyverse) # main library for data manipulation
library(haven) # for reading SPSS and STATA files
library(readxl) # for reading excel files
library(survey) # for survey analysis
library(srvyr) # for survey analysis
library(here) # for file path management
library(labelled) # for labelled data manipulation


### Load the 2019 MICS Data

mics_2019 <- read_sav("data /data/ch.sav") %>%
  as_factor() 
