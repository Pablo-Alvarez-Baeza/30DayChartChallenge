library(pacman)
p_load(tidyverse, janitor, showtext, ggtext, ggraph, igraph, camcorder)

gg_record(dir = "temp", device = "png", width = 16.5, height = 11.7, units = "in", dpi = 320)

# Fonts
font_add_google("Lato")
font_add(family = "Lato black",
         regular = "Lato-Black.ttf")

# Data
# https://ourworldindata.org/life-expectancy
life_expectancy <- read_csv("life-expectancy-of-women-vs-life-expectancy-of-women.csv") |> 
   clean_names() |> 
   select(country = entity,
          year,
          code,
          birth_female = contains("birth_females"),
          birth_male = contains("birth_males")) |> 
   filter(year == 2020,
          !is.na(code),
          !country == "World") |> 
   na.omit() |>
   pivot_longer(cols = c(birth_male, birth_female), names_to = "gender") |> 
   mutate(value = round(value, 0))
 
 # Largest gender difference = Lithuania
life_expectancy |> 
 group_by(country) |> 
 mutate(diff =  value - lag(value)) |>
 ungroup() |> 
 slice_max(diff, n = 1) 
 

# Male --------------------------------------------------------------------
life_expectancy_male <- life_expectancy |>
 filter(gender == "birth_male")  
 
life_expectancy_male |> 
  summarize(n_countries = n_distinct(country),
            max_life_expecetancy = max(value))

links_male <- data.frame(source = 0,
                         value = c(1:88))

links <- links_male |> 
  left_join(life_expectancy_male, by = "value") |> 
  mutate(country = if_else(is.na(country), "", country),
         group = factor(case_when(country == "" ~ 1,
                                  value == 70 ~ 3,
                                  TRUE ~ 2))) |> 
  select(source, destiny = value, group) |> 
  group_by(destiny) |> 
  mutate(width =n())

graph <- graph_from_data_frame(links)

# Male plot
ggraph(graph, layout="linear") + 
  geom_edge_arc(aes(edge_colour = group, edge_width = width, edge_alpha = group)) +
  geom_node_text(aes(label = name), repel = FALSE, size = 6, nudge_y = -1,
                 color = rep(c("white", NA, NA, NA), length.out = 89)) +
  scale_edge_color_manual(values = c("black","grey75", "#39FF14")) +
  scale_edge_alpha_manual(values = c(0, .15, 1)) +
  labs(title = "Life Expectancy by Sex",
       subtitle = "In every country the life expectancy of <span style='color:#BB29BB'>**women**</span> is higher than the life expectancy of <span style='color:#39FF14'>**men**</span>") +
  theme_void(base_family = "Lato") +
  theme(plot.margin = margin(t = 40, r = 50, b = 0, l = 50),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_markdown(size = 96,
                                  family = "Lato black",
                                  color = "white"),
        plot.subtitle = element_markdown(size = 28,
                                  family = "Lato",
                                  color = "white",
                                  margin = margin(t = 10, b = 100)),
        legend.position = "none")

ggsave("30chartchallenge_6_male_2022.png", width = 16.5, height = 11.7, units = "in", dpi = 320)


# Female ------------------------------------------------------------------
life_expectancy_female <- life_expectancy |> 
  filter(gender == "birth_female")  

life_expectancy_female |> 
  summarize(n_countries = n_distinct(country),
            max_life_expecetancy = max(value))


links_female <- data.frame(source = 0,
                         value = c(1:88))

links <- links_female |> 
  left_join(life_expectancy_female, by = "value") |> 
  mutate(country = if_else(is.na(country), "", country),
         group = factor(case_when(country == "" ~ 1,
                                  value == 82 ~ 3,
                                  TRUE ~ 2))) |> 
  select(source, destiny = value, group) |> 
  group_by(destiny) |> 
  mutate(width = n())

graph <- graph_from_data_frame(links)

# Female plot
ggraph(graph, layout="linear") + 
  geom_edge_arc(aes(edge_color = group, edge_width = width, edge_alpha = group), strength = -1) +
  geom_node_text(aes(label=name), repel = FALSE, nudge_y = .5,
                 color = rep(c("white", NA, NA, NA), length.out = 89)) +
  scale_edge_color_manual(values = c("black", "grey50", "#BB29BB")) +
  scale_edge_alpha_manual(values = c(0, .15, 1)) +
  labs(caption = "Visualization by Pablo Alvarez | Data from OurWorldInData: Life Expectancy at birth for the year 2020") +
  theme_void(base_family = "Lato") +
  theme(plot.margin = margin(t = 0, r = 50, b = 0, l = 50),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.caption = element_text(color = "white",
                                    size = 16,
                                    margin = margin(t = 60, b = 40),
                                    hjust = .5),
        legend.position = "none")

ggsave("30chartchallenge_6_female_2022.png", width = 16.5, height = 11.7, units = "in", dpi = 320)

# Exporting both plots to edit the last details in Figma
