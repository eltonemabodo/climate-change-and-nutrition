# Loading packges

library(tidyverse)
library(sf)
library(geodata)
library(lubridate)
library(biscale)
library(showtext)
library(cowplot)


# Custom Fonts for the plot
font_add_google("EB Garamond", "garamond")
showtext_auto()



# Getting the climate data for Zimbabwe
province_boundaries <- st_read("data /raw data/zwe_adm1_zimstat_ocha/zwe_admbnda_adm1_zimstat_ocha_20180911.shp")

# Load Precipitation data

precipitation_data <- read_xlsx("data /raw data/climate data/precipitation.xlsx") %>% 
  filter(name != "Zimbabwe") %>%
  # pivot the data
  pivot_longer(cols = -name, names_to = "date", values_to = "precipitation") %>%
  # Convert the date to a date object
  mutate(date = ym(date)) %>%
  filter(date >= ym("1995-01")) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  # filter only the farming season
  filter(month == 1 | month == 10) %>% 
  mutate(DHSYear = case_when(
    year  == 2001 | year == 2002 | year == 2003 | year == 2004 | year == 2005 ~ 2005,
    year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2010   ~ 2010,
    year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015   ~ 2015,
    TRUE ~ 2000
  )) %>% 
  filter(year <= 2015)

# Rainfall during the rain season by province and dhs year
rainfall_province <- precipitation_data %>% 
  group_by(name, DHSYear) %>% 
  summarise(rainfall = mean(precipitation, na.rm = TRUE)) 


# Max Temperature data
max_temperature_data <- read_xlsx("data /raw data/climate data/maxtemperature.xlsx") %>% 
  filter(name != "Zimbabwe") %>%
  # pivot the data
  pivot_longer(cols = -name, names_to = "date", values_to = "max_temperature") %>%
  # Convert the date to a date object
  mutate(date = ym(date)) %>%
  filter(date >= ym("1995-01")) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  mutate(DHSYear = case_when(
    year  == 2001 | year == 2002 | year == 2003 | year == 2004 | year == 2005 ~ 2005,
    year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2010   ~ 2010,
    year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015   ~ 2015,
    TRUE ~ 2000
  )) %>% 
  filter(year <= 2015)

# Max Temperature during the rain season by province and dhs year

max_temperature_province <- max_temperature_data %>% 
  group_by(name, DHSYear) %>% 
  summarise(max_temperature = mean(max_temperature, na.rm = TRUE))

## Climate Province Data 
climate_province_data <- rainfall_province %>% 
  left_join(max_temperature_province, by = c("name", "DHSYear")) 

# Join. the climate data with provincial polygons
climate_province_data <- province_boundaries %>% 
  left_join(climate_province_data, by = c("ADM1_EN" = "name")) %>% 
  st_transform(crs = 4326)

# Create biclass for the climate data

climate_province_data <- bi_class(climate_province_data, 
                                  x = "rainfall", 
                                  y = "max_temperature", 
                                  dim = 3, 
                                  style = "jenks")


pallet <- "BlueOr"
# Plot the bivariate map
climate_map <- ggplot(data = climate_province_data) +
  geom_sf(aes(fill = bi_class), color = "black") + # Map `fill` to the bivariate class
  bi_scale_fill(pal = pallet, dim = 3) +
  facet_wrap(~DHSYear, nrow = 1) +
  theme_void() +
  labs(title = "Climate Variation in Zimbabwe by Province",
       subtitle = "Average rainifall during the peak of the farming season and mean annual maximum temperature over time",
       caption = "Source: Author's calculations using World Bank Climate Data") +
  theme(
    text = element_text(family = "garamond"),
    plot.title = element_text(size = 12, face = "bold",
                              margin = margin(b = 3, l = 10)),
    plot.subtitle = element_text(size = 10, margin = margin(b = 10, l = 10)),
    plot.caption = element_text(size = 7, margin = margin(t = 10, l = 10),
                                hjust = 0),
    legend.position = "none"
  )

# Legend of bivariate map

legend <- bi_legend(pal = pallet,   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 3,
                    xlab = "Mean Annual Max Temperature (degrees)",
                    ylab = "Mean Farming Season Precipitation (mm)",
                    size = 10) +
  theme(
    text = element_text(family = "garamond", size = 12, face = "bold")
  )









