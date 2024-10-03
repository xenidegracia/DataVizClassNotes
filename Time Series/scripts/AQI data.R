#INFO 526
#In class code along examples
#Fall 2024

# load packages
if(!require(pacman))
  install.packages("pacman")

pacman::p_load(countdown,
               tidyverse,
               janitor,
               colorspace,
               broom,
               fs)

library(tidyverse)
library(janitor)
library(colorspace)
library(broom)
library(fs)

#load data - data set is part of the zipped file
path = "C:/Users/xeniadegracia/OneDrive - University of Arizona/phd/2024-Fall/DataViz/DataVizClassNotes/Time Series/scripts/data/tucson/ad_aqi_tracker_data-2022.csv"
tuc_2022 <- read_csv(here::here(path))

#Meta data 
dim(tuc_2022) #dimensions of the table

names(tuc_2022) 

#Clean the variable names 
tuc_2022 <- tuc_2022 |>
  janitor::clean_names()

names(tuc_2022)

#Take a look at the plot
ggplot(tuc_2022, aes(x = date, y = aqi_value, group = 1)) +
  geom_line()

#Plot is wonky - let's take a look at the data 
tuc_2022 |>
  select(date, aqi_value, site_name, site_id)
#it was the date form

#Transform the data - clean up date
tuc_2022 |>
  mutate(date = mdy(date))

#Investigate AQI values 
tuc_2022 |>
  distinct(aqi_value) |>
  pull()

#Start again, but 
tuc_2022 <- read_csv(
  here::here(path),
  na = c(".", "")
)

#clean the data again 
tuc_2022 <- tuc_2022 |>
  janitor::clean_names() |>
  mutate(date = mdy(date))

tuc_2022

#plot again
ggplot(tuc_2022, aes(x = date, y = aqi_value, group = 1)) +
  geom_line()

#I want to layer in the plot the AQI values so that instead of numbers there are 
#colors to indicate what level the line plot is showing

#Create an AQI level tibble 
aqi_levels <- tribble(
  ~aqi_min, ~aqi_max, ~color,    ~level,
  0,        50,       "#D8EEDA", "Good",
  51,       100,      "#F1E7D4", "Moderate",
  101,      150,      "#F8E4D8", "Unhealthy for sensitive groups",
  151,      200,      "#FEE2E1", "Unhealthy",
  201,      300,      "#F4E3F7", "Very unhealthy",
  301,      400,      "#F9D0D4", "Hazardous")

#find the midpoint between max and min 
aqi_levels <- aqi_levels |>
  mutate(aqi_mid = ((aqi_min + aqi_max) / 2))

tuc_2022 |>
  filter(!is.na(aqi_value)) |>
  ggplot(aes(x = date, y = aqi_value, group = 1)) +
  geom_rect( #Add the light colored fill to panel
    data = aqi_levels,
    aes(
      ymin = aqi_min, ymax = aqi_max,
      xmin = as.Date(-Inf), xmax = as.Date(Inf),
      fill = color, y = NULL, x = NULL
    )
  ) +
  geom_line(linewidth = 1) +
  scale_fill_identity() +
  scale_x_date(
    name = NULL, date_labels = "%b",
    limits = c(ymd("2022-01-01"), ymd("2023-03-01"))
  ) +
  geom_text(
    data = aqi_levels,
    aes(x = ymd("2023-02-28"), y = aqi_mid, label = level),
    hjust = 1, size = 6, fontface = "bold", color = "white"
  ) +
  annotate(
    geom = "text",
    x = c(ymd("2022-01-01"), ymd("2023-03-01")), y = -100,
    label = c("2022", "2023"), size = 4
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 400)) +
  labs(
    x = NULL, y = "AQI",
    title = "Ozone and PM2.5 Daily AQI Values",
    subtitle = "Tucson, AZ",
    caption = "\nSource: EPA Daily Air Quality Tracker"
  ) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(1, 1, 3, 1), "lines")
  )

aqi_levels_2 <- tribble(
  ~aqi_min, ~aqi_max, ~color,    ~level,
  0,        50,       "#D8EEDA", "Good",
  51,       100,      "#F1E7D4", "Moderate",
  101,      150,      "#F8E4D8", "Unhealthy for sensitive groups",
  151,      200,      "#FEE2E1", "Unhealthy",
  201,      300,      "#F4E3F7", "Very unhealthy",
  301,      400,      "#F9D0D4", "Hazardous")
