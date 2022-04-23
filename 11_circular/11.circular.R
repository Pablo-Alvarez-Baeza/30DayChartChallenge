library(pacman)
p_load(tidyverse, janitor, readxl, showtext, scales, camcorder)

gg_record(dir = "temp2", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)


# Fonts
font_add_google("Lato")
font_add(family = "Lato black",
         regular = "Lato-Black.ttf")


# Data
# Article about generations
# https://www.pewresearch.org/fact-tank/2019/01/17/where-millennials-end-and-generation-z-begins/ft_19-01-17_generations_2019/
df <- read_excel("transfermarkt_marketvalue.xlsx") |> 
  clean_names() |> 
  mutate(year_born = 2021- age,
         generation = factor(case_when(age >= 23 ~ "Millenials",
                                       age < 23 ~ "Generation Z")),
         position = factor(case_when(str_detect(position, "Forward|Winger") ~ "Forward",
                              str_detect(position, "Midfield") ~ "Midfielder",
                              str_detect(position, "Back") ~ "Defender")))




# Plot
df |> 
  ggplot(aes(age, market_value, fill = position)) +
  geom_point(aes(size = market_value), shape = 21, color = "white") +
  scale_size_continuous(range = c(2, 10)) +
  scale_y_continuous(limits = c(70, 170),
                     breaks = seq(70, 170, 10),
                     labels = paste0("€", seq(70, 170, 10), "M")) +
  scale_x_continuous(limits = c(17, 31),
                     breaks = seq(17, 31, 1)) +
  coord_cartesian(expand = c(0, 0)) +
  scale_fill_manual(values = c("Forward" = "#39ff14",
                               "Midfielder" = "#FF3EB5",
                               "Defender" = "#FFE900")) +
  labs(x = "Age",
       y = "Market Value",
       title = "The 25 Most Valuable Players in The World 2022",
       caption = "Visualization by Pablo Alvarez | Data from Transfermarkt") +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(rep(10, 4)),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey50", size = .1),
        axis.line = element_line(color = "grey50", size = .1),
        panel.background = element_rect(fill = "grey90", color = "grey90"),
        plot.background = element_rect(fill = "grey90", color = "grey90"),
        axis.title.y = element_text(angle = 90, hjust = 1, size = 14, color = "black"),
        axis.title.x = element_text(hjust = 0, size = 14, color = "black"),
        axis.text = element_text(color = "grey50", size = 12),
        plot.title = element_text(family = "Lato black",
                                  color = "black",
                                  size = 30,
                                  hjust = 0,
                                  margin = margin(b = 20)),
        plot.caption = element_text(size = 12,
                                    color = "black",
                                    hjust = 0,
                                    margin = margin(t = 10)),
        legend.position = "none"
  )

ggsave("30chartchallenge_11_circular_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)

  

