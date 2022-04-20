library(pacman)
p_load(tidyverse, showtext, ggtext, camcorder)

# Convert pixels to inches
# https://www.pixelto.net/px-to-inches-converter
# gg_record(dir = "temp", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)


# Fonts
font_add_google("Lato")
font_add(family = "Lato black",
         regular = "Lato-Black.ttf")
font_add(family = "Lato light",
         regular = "Lato-Light.ttf")

# Data
# https://ourworldindata.org/fertility-rate
df <- read_csv("children-per-woman-UN.csv") |> 
  clean_names() |> 
  select(country = entity, year, fertility_rate = starts_with("estimates"))

# Sharpest decrease
df |> 
  filter(year %in% c(1950, 2020)) |>
  group_by(country) |> 
  mutate(diff = fertility_rate - lag(fertility_rate)) |>
  ungroup() |> 
  na.omit() |> 
  slice_min(diff, n = 1) |> View()

# Plot
df |> 
  ggplot(aes(year, fertility_rate, group = country)) +
  geom_line(data = df |> filter(!country %in% c("Taiwan", "World")), color = "grey75", alpha = .25, size = .25) +
  geom_line(data = df |> filter(country == "Taiwan"), color = "black", size = .25, alpha = .5) +
  geom_line(data = df |> filter(country == "World"), color = "white", size = 1.25) +
  geom_line(data = df |> filter(country == "World"), color = "#BB29BB", size = 1) +
  scale_x_continuous(limits = c(1950, 2025.5),
                     breaks = seq(1950, 2025.5, by = 10)) +
  scale_y_continuous(limits = c(1, 8.5),
                     breaks = seq(1, 9, by = 1)) +
  geom_text(data = df |> filter(country == "World", year == 2020), aes(label = paste(country, "\n", round(fertility_rate, 1))), x = 2020.25, color = "#BB29BB", family = "Lato", hjust = 0, vjust = .75, size = 1.5, lineheight = .9, fontface = "bold") +
  geom_text(data = df |> filter(country == "Taiwan", year == 2020), aes(label = paste(country, "\n", round(fertility_rate, 1))), x = 2020.25, color = "black", family = "Lato", hjust = 0,  vjust = .4, size = 1.5, lineheight = .9) +
  coord_cartesian(xlim = c(1950, 2025.5),
                  clip = "off",
                  expand = c(0, 0)) +
  theme_minimal(base_family = "Lato") +
  labs(x = NULL,
       y = "Fertility rate",
       title = "The Global Decline of the Fertility Rate",
       subtitle = "70 years ago the average woman had five children, since then the number has halved",
       caption = "Visualization by Pablo Alvarez | Data from Our World In Data\nFertility rate measures the average number of children per woman") +
  theme(
    panel.grid = element_blank(),
    panel.margin = margin(10,5, 10, 5),
    plot.background = element_rect(fill = "#E5F0DE", color = "#E5F0DE"),
    panel.background = element_rect(fill = "#E5F0DE", color = "#E5F0DE"),
    axis.title.y = element_text(angle = 90, hjust = .95, size = 4),
    axis.text = element_text(size = 4.5),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.line = element_line(color = "grey50", size = .1),
    plot.title = element_markdown(size = 11,
                              color = "#BB29BB",
                              family = "Lato black",
                              hjust = 0),
    plot.subtitle = element_text(size = 6,
                              color = "#BB29BB",
                              family = "Lato",
                              hjust = 0,
                              margin = margin(b = 20, t = -3.5)),
    plot.caption = element_text(size = 4,
                                color = "black",
                                hjust = 0,
                                margin = margin(t = 20))
  )

ggsave("30chartchallenge_19_global_change_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)

# gg_playback()
  