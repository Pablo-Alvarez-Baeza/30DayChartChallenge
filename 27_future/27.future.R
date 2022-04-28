library(pacman)

p_load(tidyverse, janitor, camcorder, showtext, ggrepel)
gg_record(dir = "temp_future", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)

# Fonts
font_add_google("Lato")
font_add(family = "Lato bold",
         regular = "Lato-Bold.ttf")

showtext_auto()


# Data --------------------------------------------------------------------
df <- read_csv("population-of-all-world-regions-including-the-un-projection-until-2100.csv") |> 
  clean_names() |> 
  select(entity, year, world_pop = starts_with("estimate"), world_pop_projection = starts_with("medium")) |> 
  filter(entity %in% c("Europe", "Africa", "Asia", "Oceania", "South America", "Northern America")) |> 
  mutate(group = factor(if_else(is.na(world_pop), 1, 2)),
         world_pop = world_pop / 1e9,
         world_pop_projection = world_pop_projection / 1e9,
         world_pop = if_else(is.na(world_pop), world_pop_projection, world_pop)) |> 
  select(-world_pop_projection)
  
world_pop_labels <- df |> 
  filter(year == 2100) |> 
  arrange(-world_pop) |> 
  mutate(labels = if_else(!entity %in% c("Asia", "Africa"),
                                  paste(entity, round(world_pop * 1000, 2), "million"),
                                  paste(entity, round(world_pop * 1, 2), "billion"))) 


# Plot --------------------------------------------------------------------
df |>
  ggplot(aes(year, world_pop, color = entity)) +
  geom_segment(
    data = tibble(y = seq(0, 6, by = 2), x1 = 1950, x2 = 2100),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey90",
    size = .2
  ) +
  geom_segment(x = 1950, xend = 2100, y = 0, yend = 0,
               color = "grey90", size = .2) +
  geom_line(data = df |> filter(group == 2, !entity == "Africa")) +
  geom_line(data = df |> filter(group == 2, entity == "Africa")) +
  geom_line(data = df |> filter(group == 1), linetype = "dashed", size = .1) +
  scale_x_continuous(limits = c(1950, 2195),
                     breaks = seq(1950, 2100, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, 2),
                     labels = c("0", paste(seq(2, 6, 2), "billion")),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("#39FF14", rep("grey60", 5))) +
  geom_text_repel(
    data = df |> filter(year == 2100),
    aes(color = entity, label = c(world_pop_labels$labels[2],
                                  world_pop_labels$labels[1],
                                  world_pop_labels$labels[3:4],
                                  world_pop_labels$labels[c(6, 5)])),
    family = "Lato",
    fontface = "bold",
    size = 4.5,
    xlim = c(2115, NA),
    hjust = 0,
    direction = 'y',
    segment.size = .05,
    segment.linetype = "solid"
  ) +
  labs(x = NULL,
       y = "World population",
       title = "Africa 2100",
       subtitle = "Africa's population is projected to grow by 3.3 times by the end of the century.\nIts share of the world population will rise from 17% to 40%",
       caption = "Visualization by Pablo Alvarez | Data from Our World In Data") +
  theme_minimal(base_family = "Lato") +
  theme(legend.position = "none",
        plot.margin = margin(rep(5,4)),
        plot.background = element_rect(fill = "grey95", color = "grey95"),
        panel.background = element_rect(fill = "grey95", color = "grey95"),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = "grey90", size = .2),
        axis.title.y = element_text(size = 12, color = "grey25", angle = 90, hjust = 1),
        axis.text = element_text(size = 12, color = "grey25"),
        axis.text.y = element_text(margin = margin(r = 0)),
        plot.title = element_text(size = 64,
                                      color = "#39FF14",
                                      family = "Lato bold",
                                      hjust = 0),
        plot.subtitle = element_text(size = 18,
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
        
