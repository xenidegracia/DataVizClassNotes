#INFO 526
#In class code along examples
#Fall 2024

#Arrange your data 
tuc_2022 |>
  select(date, aqi_value) |>
  filter(!is.na(aqi_value)) |>
  arrange(date)

#Identify good days 
tuc_2022 |>
  select(date, aqi_value) |>
  filter(!is.na(aqi_value)) |>
  arrange(date) |>
  mutate(good_aqi = if_else(aqi_value <= 50, 1, 0))

#Sum over time 
tuc_2022 |>
  select(date, aqi_value) |>
  filter(!is.na(aqi_value)) |>
  arrange(date) |>
  mutate(
    good_aqi = if_else(aqi_value <= 50, 1, 0),
    cumsum_good_aqi = cumsum(good_aqi)
  )

#Plotting cumulatives 
tuc_2022 |>
  select(date, aqi_value) |>
  filter(!is.na(aqi_value)) |>
  arrange(date) |>
  mutate(
    good_aqi = if_else(aqi_value <= 50, 1, 0),
    cumsum_good_aqi = cumsum(good_aqi)
  ) |>
  ggplot(aes(x = date, y = cumsum_good_aqi, group = 1)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y") +
  labs(
    x = NULL, y = "Number of days",
    title = "Cumulative number of good AQI days (AQI < 50)",
    subtitle = "Tucson, AZ",
    caption = "\nSource: EPA Daily Air Quality Tracker"
  ) +
  theme(plot.title.position = "plot")

