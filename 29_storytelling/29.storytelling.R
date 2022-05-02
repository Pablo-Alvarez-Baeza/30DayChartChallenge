library(pacman)

p_load(tidyverse, janitor, camcorder, showtext, ggstream)
gg_record(dir = "temp_storytelling", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)


# Fonts
font_add_google("Lato")
font_add(family = "Lato bold",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Bold.ttf")

showtext_auto()


# Data --------------------------------------------------------------------
df <- read_csv("people-living-in-democracies.csv") |> 
  clean_names() |> 
  pivot_longer(cols = contains("population"), names_to = "political_system", values_to = "population") |> 
  mutate(political_system = factor(str_remove(political_system, "population_"))) |> 
  select(political_system, year, population)


# Plot --------------------------------------------------------------------
df |> 
  ggplot(aes(year, population, color = political_system, fill = political_system)) +
  geom_stream(color = "white", lwd = 0.2, bw = .5) +
  scale_x_continuous(limits = c(1800, 2021),
                     breaks = seq(1800, 2021, 20)) +
  labs(title = "200 years ago, everyone lacked democratic rights,\nnow, billions of people have them",
       subtitle = "Number of people living in political regimes (1800 - 2021) based on the criteria of the\nclassification by Lührmann et al. (2018) and the assessment by V-Dem's experts",
       caption = "Visualization by Pablo Alvarez | Data from Our World In Data") +
  scale_fill_manual(values = c("#E72F52", "#EFB743", "#0D95D0", "#7DC462", "grey75")) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.margin = margin(rep(5,4)),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12.5, color = "grey50"),
    axis.line.x = element_line(color = "grey90", size = .2),
    plot.title = element_text(size = 32,
                              color = "black",
                              lineheight = .3,
                              family = "Lato bold",
                              hjust = 0,
                              margin = margin(b = 0)),
    plot.subtitle = element_text(size = 18,
                                 color = "grey25",
                                 family = "Lato",
                                 hjust = 0,
                                 lineheight = .3,
                                 margin = margin(b = 40, t = .5)),
    plot.caption = element_text(size = 12,
                                color = "grey25",
                                hjust = 0,
                                margin = margin(t = 20)),
    legend.position = "none"
  )

ggsave("30chartchallenge_29_storytelling_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)

gg_playback()
