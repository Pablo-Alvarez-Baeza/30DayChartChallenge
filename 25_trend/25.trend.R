library(pacman)

p_load(tidyverse, janitor, camcorder, showtext, viridis)
gg_record(dir = "temp_trend", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)

# Fonts
font_add_google("Lato")
font_add(family = "Lato bold",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Bold.ttf")

showtext_auto()


# Data --------------------------------------------------------------------
df <- read_csv("ipcc-scenarios.csv") |> 
  clean_names()

df_temp <- df |> 
  select(scenario, year,  contains("temperature")) |> 
  mutate(group = factor(if_else(str_detect(scenario, "SSP1 - Baseline"), 1, 2)))


# Plot --------------------------------------------------------------------
df_temp |> 
  ggplot() +
  geom_line(data = df_temp |> filter(year <= 2020,
                                     scenario == "SSP1 - Baseline"),
            aes(year, temperature), color = "black", size = .75) +
  geom_line(data = df_temp |> filter(year >= 2020),
            aes(year, temperature, group = scenario), color = "lightblue", size = .25) +
  geom_line(data = df_temp |> filter(year >= 2020, group == 1),
            aes(year, temperature, group = scenario), size = .35, color = "black", linetype = "dashed") +
  #scale_color_viridis(begin = 1, end = 0, alpha = .5, option = "viridis") +
  scale_x_continuous(limits = c(2005, 2105),
                     breaks = c(2005, seq(2020, 2105, 10))) +
  scale_y_continuous(position = "right",
                     limits = c(0, 6),
                     breaks = seq(0, 5, 1),
                     labels = c("0", paste0("+", c(1:4)), "+5C")) +
  annotate(geom = "text", x = 2040, y = 3.01, lineheight = .25, hjust = 0, size = 6, family = "Lato", label = "SSP1: Sustainability – Taking the Green Road\nprovides the most positive scenario for\nboth human development and environmental action") +
  coord_cartesian(clip = "off", xlim = c(2005, 2100), ylim = c(0, 5), expand = c(0, 0)) +
  labs(title = "Global average temperature increase relative to the pre-industrial era",
       subtitle = "IPCC Scenarios for the global average temperature increase relative\nto the pre-industrial era, which is taken to be the year 1750",
       caption = "Visualization by Pablo Alvarez | Data from Our World In Data") +
    theme_minimal(base_family = "Lato") +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          plot.margin = margin(rep(5, 4)),
          axis.line.x = element_line(color = "grey90", size = .2),
          axis.text.x = element_text(color = "grey50", size = 12),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title = element_blank(),
          plot.title = element_text(size = 24,
                                    color = "black",
                                    family = "Lato bold",
                                    hjust = 0),
          plot.subtitle = element_text(size = 14,
                                       color = "grey25",
                                       family = "Lato",
                                       hjust = 0,
                                       lineheight = .3,
                                       margin = margin(b = 20, t = .5)),
          plot.caption = element_text(size = 10.5,
                                      color = "grey25",
                                      hjust = 0,
                                      margin = margin(t = 20))
    )


#geom_richtext(geom = "text", x = 2100.5, y = 4.8, family = "Lato", fill = NA, label.color = NA, lineheight = .25, hjust = 0, label = "**SSP5 Fossil-fueled Development – Taking the Highway**\nis the most pessimist scenario, leading to continued\nlarge negative effects on the environment") +
#geom_richtext(geom = "text", x = 2100.5, y = 2.8, family = "Lato", fill = NA, label.color = NA, lineheight = .25, hjust = 0, label = "**SSP1: Sustainability – Taking the Green Road**\nprovides the most positive scenario\nfor both human development and environmental action with an increase of 3.02°C") +