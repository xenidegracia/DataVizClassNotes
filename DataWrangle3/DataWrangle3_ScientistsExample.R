#Data Wrangling example
#Top Ten Female Scientists
#INFO 526
#Fall 2024

#Set up 
if(!require(pacman))
  install.packages("pacman")

pacman::p_load(countdown,
               tidyverse,
               glue,
               scales,
               ggthemes,
               gt,
               palmerpenguins,
               openintro,
               ggrepel)

#Read in data 
professions <- read_csv("data/professions.csv")
dates <- read_csv("data/dates.csv")
works <- read_csv("data/works.csv")

#Combine the three tables 
scientists <- professions |>
  left_join(dates) |>
  left_join(works)

#clean data in table and fill in missing values 
scientists_longer <- scientists |>
  mutate(
    birth_year = case_when(
      name == "Ada Lovelace" ~ 1815,
      name == "Marie Curie" ~ 1867,
      TRUE ~ birth_year
    ),
    death_year = case_when(
      name == "Ada Lovelace" ~ 1852,
      name == "Marie Curie" ~ 1934,
      name == "Flossie Wong-Staal" ~ 2020,
      TRUE ~ death_year
    ),
    status = if_else(is.na(death_year), "alive", "deceased"),
    death_year = if_else(is.na(death_year), 2021, death_year),
    known_for = if_else(name == "Rosalind Franklin", "understanding of the molecular structures of DNA ", known_for)
  ) |>
  pivot_longer(
    cols = contains("year"),
    names_to = "year_type",
    values_to = "year"
  ) |>
  mutate(death_year_fake = if_else(year == 2021, TRUE, FALSE))

#Visualize the data
ggplot(scientists_longer, 
       aes(x = year, y = fct_reorder(name, as.numeric(factor(profession))), 
           group = name, color = profession)) +
  geom_point(aes(shape = death_year_fake), show.legend = FALSE) +
  geom_line(aes(linetype = status), show.legend = FALSE) +
  scale_shape_manual(values = c("circle", NA)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_colorblind() +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(1820, 2020, 50)) +
  geom_text(aes(y = name, label = known_for), x = 2030, show.legend = FALSE, hjust = 0) +
  geom_text(aes(label = profession), x = 1809, y = Inf, hjust = 1, vjust = 1, show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Year", y = NULL,
    title = "10 women in science who changed the world",
    caption = "Source: Discover magazine"
  ) +
  facet_grid(profession ~ ., scales = "free_y", space = "free_y", switch = "x") +
  theme(
    plot.margin = unit(c(1, 23, 1, 4), "lines"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 2), # manual hack
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_text(hjust = 0),
    panel.background = element_rect(fill = "#f0f0f0", color = "white"),
    panel.grid.major = element_line(color = "white", size = 0.5)
  )

