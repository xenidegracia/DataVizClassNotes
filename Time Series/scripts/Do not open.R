###DO NOT PEEK 

#Solution to recreation in class activity 







tuc_2022 |>
  filter(!is.na(aqi_value)) |>
  ggplot(aes(x = date, y = aqi_value, group = 1)) +
  geom_line(linewidth = 1) +
  scale_x_date(
    name = NULL, date_labels = "%b",
    limits = c(ymd("2022-01-01"), ymd("2023-03-01"))
  ) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 300, 400)) +
  geom_text(
    data = aqi_levels,
    aes(x = ymd("2023-02-28"), y = aqi_mid, label = level, color = darken(color, 0.3)),
    hjust = 1, size = 6, fontface = "bold"
  ) +
  scale_color_identity() +
  annotate(
    geom = "text",
    x = c(ymd("2022-01-01"), ymd("2023-03-01")), y = -80,
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
    panel.grid.minor.x = element_blank()
  )
