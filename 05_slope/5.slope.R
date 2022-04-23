library(pacman)
p_load(tidyverse, showtext, ggtext)


# Fonts -------------------------------------------------------------------
font_add_google("Lato")
font_add(family = "Lato Bold",
         regular = "Lato-Bold.ttf")


# Data --------------------------------------------------------------------
players_22 <- read_csv("players_22.csv") |> 
  select(sofifa_id, short_name, overall) |> 
  rename_at(vars(-c(sofifa_id, short_name)), paste0, "_2022")  
  
players_18 <- read_csv("players_18.csv") |> 
  select(sofifa_id, short_name, overall) |> 
  rename_at(vars(-c(sofifa_id, short_name)), paste0, "_2018") |> 
  filter(overall_2018 >= 83)
  
players <- players_18 |> 
  left_join(players_22, by = "sofifa_id") |> 
  na.omit() |> 
  mutate(diff = overall_2022 - overall_2018) |> 
  pivot_longer(cols = starts_with("overall"), names_to = "year", values_to = "overall") |> 
  select(-short_name.x, short_name = short_name.y) |> 
  mutate(year = if_else(year == "overall_2018", 2018, 2022))

overall_size <- players |> 
  group_by(year) |> 
  count(overall)

players <- players |> 
  left_join(overall_size) 

# n players with a 83 rating in 2018 = 70
players_18 |> 
  filter(overall_2018 == 83) |> 
  distinct(short_name)

# n players with a 91 rating in 2022 = 5
players_22 |> 
  filter(overall_2022 == 91) |> 
  distinct(short_name) |> 
  arrange(short_name)

# n players in 2018 with a higher rating than Mbappe
players_18 |> 
  filter(overall_2018 > 83) |> 
  count()

# n players in 2022 with a higher rating than Mbappe
players_22 |> 
  filter(overall_2022 > 91) 


# Plot --------------------------------------------------------------------
players |> 
  ggplot(aes(year, overall, group = short_name)) +
  geom_line(data = players |> filter(!short_name == "K. Mbappé"), color = "grey85", size = 1, alpha = .5) +
  geom_point(data = players |> filter(!short_name == "K. Mbappé"), aes(size = n), color = "white", alpha = .5, fill = "grey85", shape = 21) +
  geom_line(data = players |> filter(short_name == "K. Mbappé"), color = "#611EAF", size = 2) +
  geom_point(data = players |> filter(short_name == "K. Mbappé"), aes(size = n), color = "white", fill = "#611EAF", shape = 21) +
  scale_y_continuous(limits = c(80, 95)) +
  scale_x_continuous(limits = c(2017, 2023),
                     labels = c("", "2018\nOverall Rating", rep("", 3), "2022\nOverall Rating", "")) +
  geom_text(data = players |> filter(year == 2018, overall > 83), aes(label = overall), hjust = 2.5, color = "white", size = 2, family = "Lato") +
  geom_text(data = players |> filter(year == 2018, overall == 83), aes(label = overall), hjust = 2.5, color = "#611EAF", size = 2, family = "Lato") +
  geom_text(data = players |> filter(year == 2022, !overall == 91), aes(label = overall), hjust = -1, color = "white", size = 2, family = "Lato") +
  geom_text(data = players |> filter(year == 2022, overall == 91), aes(label = overall), hjust = -1, color = "#611EAF", size = 2, family = "Lato") +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  labs(title = "The Meteoric Rise of Kylian Mbappé",
       subtitle = "As of 2018, he was the 128th highest-rated player in EA SPORTS FIFA.<br>In just four years he has risen to become the **third highest-rated one**",
       caption = "Visualization by Pablo Alvarez | Data from Kaggle") +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(10, 15, 10, 15),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#A4D4C3", color = "#A4D4C3"),
        plot.background = element_rect(fill = "#A4D4C3", color = "#A4D4C3"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 4.5, color = "white", margin = margin(t = 5)),
        plot.title = element_text(family = "Lato black",
                                  size = 12.5,
                                  color = "#611EAF"),
        plot.subtitle = element_markdown(size = 6.5, margin = margin(b = 30), color = "#611EAF", lineheight = 1.25),
        plot.caption = element_text(size = 4, hjust = .5, margin = margin(t = 20), color = "#611EAF"),
        legend.position = "none")
  
ggsave("30chartchallenge_5_slope_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)
  