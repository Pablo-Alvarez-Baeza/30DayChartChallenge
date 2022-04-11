library(pacman)
p_load(tidyverse, showtext, ggtext)

font_add_google("Lato")
font_add(family = "Lato black",
         regular = "Lato-Black.ttf")


# Data
# https://www.tylervigen.com/spurious-correlations
# Useful source about side-by-side charts
# https://blog.datawrapper.de/dualaxis/

df <- data.frame(
  year = c(1999:2009),
  nc = c(2, 2, 2, 3, 1, 1, 2, 3, 4, 1, 4),
  drowings = c(109, 102, 102, 98, 85, 95, 96, 98, 124, 94, 102)
) |> 
  pivot_longer(cols = c(nc, drowings), names_to = "variable") |> 
  mutate(variable = factor(variable))


# Plot
df |> 
  ggplot(aes(year, value)) +
  geom_point(color = "black", size = .5) +
  geom_line(color = "black", size = .1) +
  facet_wrap(~ variable, scales = "free",
             strip.position = "top",
             labeller = as_labeller(c(drowings = "Swimming pool drownings", nc = "Films Nicolas Cage appeared in"))) +
  scale_x_continuous(limits = c(1998, 2009),
                     breaks = seq(1998, 2009, by = 1),
                     labels = c(1998, rep("", 5), 2004, rep("", 4), 2009),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Nicolas Cage Film Appearances vs\nNumber of People Who Drowned by Falling Into a Pool",
       subtitle = "The number of people who drowned by falling into a pool correlates with the number\nof films Nicolas Cage appeared in",
       caption = "Visualization by Pablo Alvarez | Data from U.S. Office of Management and Budget and Centers for Disease Control & Prevention") +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#A0D1D6", color = "#A0D1D6"),
        axis.text = element_text(size = 4),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white", size = .1), 
        axis.ticks.x = element_line(color = c("white", rep(NA, 5), "white", rep(NA, 4), "white"), size = .1),
        axis.ticks.y = element_line(size = .1),
        panel.spacing = unit(2.5, "lines"),
        strip.text =  element_text(family = "Lato", size = 5, hjust = 0),
        axis.title = element_blank(),
        plot.title = element_text(family = "Lato black",
                                  size = 8.5,
                                  color = "black",
                                  lineheight = .9),
        plot.subtitle = element_text(size = 5.75, margin = margin(b = 30)),
        plot.caption = element_text(size = 3.5, hjust = .5, margin = margin(t = 20)),
        legend.position = "none")

ggsave("30chartchallenge_13_correlations_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)
        