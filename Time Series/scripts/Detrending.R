#INFO 526
#In class code along examples
#Fall 2024

#there are multiple years of tucson, AZ data 
tuc_files <- fs::dir_ls(here::here("data/tucson"))
tuc_files

#Reading multiple files 
tuc <- read_csv(tuc_files, na = c(".", ""))

tuc <- tuc |>
  janitor::clean_names() |>
  mutate(
    date = mdy(date),
    good_aqi = if_else(aqi_value <= 50, 1, 0)
  ) |>
  filter(!is.na(aqi_value)) |>
  arrange(date) |>
  mutate(cumsum_good_aqi = cumsum(good_aqi), .after = aqi_value)

tuc

#Plot trends since 2016
tuc |>
  ggplot(aes(x = date, y = cumsum_good_aqi, group = 1)) +
  geom_smooth(method = "lm", color = "pink") +
  geom_line() +
  scale_x_date(
    expand = expansion(mult = 0.02),
    date_labels = "%Y"
  ) +
  labs(
    x = NULL, y = "Number of days",
    title = "Cumulative number of good AQI days (AQI < 50)",
    subtitle = "Tucson, AZ",
    caption = "\nSource: EPA Daily Air Quality Tracker"
  ) +
  theme(plot.title.position = "plot")

#To detrend we fit a linear regression line, augment the data with model results 
# divide the observed value by the respective value in the long term trend

# fit a linear regression line 
m <- lm(cumsum_good_aqi ~ date, data = tuc)
m

#augment the data with the model results 
tuc_aug <- augment(m)
tuc_aug

#divide the observed value of cumsum_good_aqi by the respective value in the long
#term trend
tuc_aug <- tuc_aug |>
  mutate(ratio = cumsum_good_aqi / .fitted, .after = .fitted)
tuc_aug

#Visualize detrended data 
tuc_aug |>
  ggplot(aes(x = date, y = ratio, group = 1)) +
  geom_hline(yintercept = 1, color = "gray") +
  geom_line() +
  scale_x_date(
    expand = expansion(mult = 0.1),
    date_labels = "%Y"
  ) +
  labs(
    x = NULL, y = "Number of days\n(detrended)",
    title = "Cumulative number of good AQI days (AQI < 50)",
    subtitle = "Tucson, AZ",
    caption = "\nSource: EPA Daily Air Quality Tracker"
  ) +
  theme(plot.title.position = "plot")

#Barely anything happened - let's look at some more interesting data - SAN FRAN!

#make an object with a list of files in the directory
sf_files <- fs::dir_ls(here::here("data/sanfran"))

#read the files into one tibble
sf <- read_csv(sf_files, na = c(".", ""))

#clean it up
sf <- sf |>
  janitor::clean_names() |>
  mutate(
    date = mdy(date),
    good_aqi = if_else(aqi_value <= 50, 1, 0)
  ) |>
  filter(!is.na(aqi_value)) |>
  arrange(date) |>
  mutate(cumsum_good_aqi = cumsum(good_aqi), .after = aqi_value)
#take a look at the data
sf

#Plot the trend 
sf |>
  ggplot(aes(x = date, y = cumsum_good_aqi, group = 1)) +
  geom_smooth(method = "lm", color = "pink") +
  geom_line() +
  scale_x_date(
    expand = expansion(mult = 0.07),
    date_labels = "%Y"
  ) +
  labs(
    x = NULL, y = "Number of days",
    title = "Cumulative number of good AQI days (AQI < 50)",
    subtitle = "San Francisco-Oakland-Hayward, CA",
    caption = "\nSource: EPA Daily Air Quality Tracker"
  ) +
  theme(plot.title.position = "plot")

#Detrend 
#Fit a simple linear regression
m_sf <- lm(cumsum_good_aqi ~ date, data = sf)

#augment the data with the model results 
sf_aug <- augment(m_sf)

#divide the observed value by the respective value in the long-term trend 
sf_aug <- sf_aug |>
  mutate(ratio = cumsum_good_aqi / .fitted, .after = .fitted)

#Visualize detrended data! 
sf_aug |>
  ggplot(aes(x = date, y = ratio, group = 1)) +
  geom_hline(yintercept = 1, color = "gray") +
  geom_line() +
  scale_x_date(
    expand = expansion(mult = 0.07),
    date_labels = "%Y"
  ) +
  labs(
    x = NULL, y = "Number of days\n(detrended)",
    title = "Cumulative number of good AQI days (AQI < 50)",
    subtitle = "San Francisco-Oakland-Hayward, CA",
    caption = "\nSource: EPA Daily Air Quality Tracker"
  ) +
  theme(plot.title.position = "plot")
