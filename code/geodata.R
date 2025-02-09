# Loading packges

library(tidyverse)
library(sf)
library(geodata)
library(lubridate)


# Getting the climate data for Zimbabwe
province_boundaries <- st_read("data /raw data/zwe_adm1_zimstat_ocha/zwe_admbnda_adm1_zimstat_ocha_20180911.shp")

# Load Precipitation data

precipitation_data <- read_xlsx("data /raw data/climate data/precipitation.xlsx") %>% 
  filter(name != "Zimbabwe") %>%
  # pivot the data
  pivot_longer(cols = -name, names_to = "date", values_to = "precipitation") %>%
  # Convert the date to a date object
  mutate(date = ym(date)) %>%
  filter(date >= ym("2000-01")) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  # filter only the farming season
  filter(month == 1 | month == 10) %>% 
  mutate(DHSYear = case_when(
    year == 2000 | year == 2001 | year == 2002 | year == 2003 | year == 2004 | year == 2005 ~ 2005,
    year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2010   ~ 2010,
    year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015   ~ 2015,
    TRUE ~ NA
  )) %>% 
  filter(!is.na(DHSYear))

# Rainfall during the rain season by province and dhs year
rainfall_province <- precipitation_data %>% 
  group_by(name, DHSYear) %>% 
  summarise(rainfall = mean(precipitation, na.rm = TRUE)) 

# Merge the rainfall data with the province boundaries
province_boundaries <- left_join(province_boundaries, rainfall_province, by = c("ADM1_EN" = "name"))
  
# Plot the rainfall data
ggplot(province_boundaries) +
  geom_sf(aes(fill = rainfall)) +
  facet_wrap(~DHSYear) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Average Rainfall by Province",
       fill = "Rainfall (mm)",
       caption = "Source: Zimbabwe Meteorological Services") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5))
