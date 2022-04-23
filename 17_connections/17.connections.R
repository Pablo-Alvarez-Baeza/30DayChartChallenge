library(pacman)
p_load(tidyverse, janitor, readxl, showtext, camcorder)

# Convert pixels to inches
# https://www.pixelto.net/px-to-inches-converter
gg_record(dir = "temp", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)


# Fonts
font_add_google("Lato")
font_add(family = "Lato black",
         regular = "Lato-Black.ttf")


# Data
bm <- read_excel("black_mirror_imdb.xlsx") |>
  clean_names() |> 
  mutate(episode_number = 1:n(),
         season = factor(season))


# Plot
bm |> 
  ggplot(aes(episode_number, imdb_rating, color = season)) +
  geom_point() +
  geom_smooth(size =.1, show.legend = FALSE) +
  scale_x_continuous(limits = c(.5, 23.5),
                     breaks = seq(1, 23, 3)) +
  scale_y_continuous(limits = c(6, 10),
                     breaks = seq(6, 10, 1)) +
  coord_cartesian(expand = c(0, 0)) +
  scale_color_manual(values = c("#FF3EB5", "#50AB9F", "#F93822", "#39FF14", "white", "#FFE900")) +
  theme_minimal(base_family = "Lato") +
  guides(color = guide_legend(nrow = 1, override.aes=list(fill=NA))) +
  labs(x = "Episode Number",
       y = "IMDb Rating",
       color = "Season",
       title = "Black Mirror",
       subtitle = "IMDb ratings for episodes of every season",
       caption = "Visualization by Pablo Alvarez | Data from IMDb") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey75", size = .1),
    plot.margin = margin(rep(10, 4)),
    axis.title.y = element_text(angle = 90, hjust = .95, size = 4, color = "white"),
    axis.title.x = element_text(hjust = 0, size = 4, color = "white"),
    axis.text = element_text(size = 4.5, color = "white"),
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(family = "Lato black",
                              color = "white",
                              size = 18,
                              hjust = 0),
    plot.subtitle = element_text(size = 7,
                                 color = "white",
                                 family = "Lato",
                                 hjust = 0,
                                 margin = margin(b = 40, t = -3.5)),
    plot.caption = element_text(size = 4.5,
                                color = "white",
                                hjust = 0,
                                margin = margin(t = 20)),
    legend.position = c(.2, 1.15),
    legend.text = element_text(color = "white", size = 4),
    legend.title = element_text(color = "white", size = 6, face="bold"),
    legend.spacing.x = unit(.1, 'cm'),
    legend.key.size = unit(.1, "cm")
  )
  
ggsave("30chartchallenge_18_connections_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)

gg_playback()
