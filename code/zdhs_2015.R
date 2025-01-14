## Loading all the necessary libraries

library(tidyverse) # main library for data manipulation
library(haven) # for reading SPSS and Stata files
library(readxl) # for reading excel files


# Reading the data
zdhs_2015 <- read_dta("climate-change-and-nutrition/data /raw data/zdhs_2015.DTA")
