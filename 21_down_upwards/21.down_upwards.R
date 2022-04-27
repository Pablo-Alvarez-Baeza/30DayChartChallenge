library(pacman)

# devtools::install_github("bradleyboehmke/harrypotter")
p_load(tidyverse, janitor, tidytext, harrypotter, camcorder, showtext, ggfx)
gg_record(dir = "temp_down_upwards", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)

# Fonts
font_add_google("Lato")
font_add(family = "harrypotter",
         regular = "harrypotter.ttf")

showtext_auto()


# Sources -----------------------------------------------------------------
# Tutorial by Paul Van der Laken
# https://paulvanderlaken.com/2017/08/03/harry-plotter-celebrating-the-20-year-anniversary-with-tidytext-the-tidyverse-and-r/

# R studio tutorial
# https://rstudio-pubs-static.s3.amazonaws.com/300624_8260952d1f0346969e65f41a97006bf5.html

# Cedric's code
# https://github.com/z3tt/30DayChartChallenge/blob/main/04_magical/04_magical.R


# Data --------------------------------------------------------------------
goblet_of_fire <- harrypotter::goblet_of_fire
  
text_gof <- tibble(chapter = factor(seq_along(goblet_of_fire)),
       text = goblet_of_fire)

words_gof <- text_gof |>
  unnest_tokens(word, text) |> 
  anti_join(stop_words) |> 
  inner_join(get_sentiments("afinn"), by = "word") |> 
  filter(value != 0) 

# Overall score per chapter
words_gof_plot <- words_gof |> 
  group_by(chapter) |> 
  summarize(score = sum(value)) |> 
  ungroup() |> 
  mutate(sentiment = factor(if_else(score < 0, "negative", "positive")))

# y coordinates for annotations
annotations <- words_gof_plot |> 
  group_by(chapter) |> 
  mutate(score2 = if_else(score > 0, 0, score),
         yend = score2 - 5) |> 
  ungroup() |> 
  select(yend)

# x values for axis x colors
axis_x_color <- words_gof_plot |> 
  group_by(chapter) |> 
  mutate(score2 = if_else(score > 0, "#F93822", "#39FF14")) |> 
  ungroup() |> 
  pull()

# checking out which chapters are positive
words_gof_plot |> 
  filter(sentiment == "positive") 


# Plot --------------------------------------------------------------------
words_gof_plot |> 
ggplot(aes(chapter, score, group = TRUE)) +
  with_outer_glow(geom_col(data = filter(words_gof_plot, score > 0), fill = "white", width = .75),
                  colour = "#F93822", sigma = 7.5) +
  scale_x_discrete(drop = FALSE) +
  with_outer_glow(geom_col(data = filter(words_gof_plot, score < 0), fill = "white", width = .75),
                  colour = "#39FF14", sigma = 7.5) +
  geom_hline(yintercept = 0, size = .25, color = "white") +
  scale_y_continuous(limits = c(-250, 250),
                     breaks = seq(-250, 250, 50),
                     labels = c("", "-200", "", "-100", "", "0", "", "100", "", "200", "")) +
  coord_cartesian(clip = "off") +
  annotate("segment", x = 0, xend = 0, y = 2.5, yend = 50,
           colour = "#F93822", size = .05, lineend = "round", linejoin = "bevel",
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("segment", x = 0, xend = 0, y = -2.5, yend = -50,
           colour = "#39FF14", size = .05, lineend = "round", linejoin = "bevel",
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = -.5, y = 35, label = "Positive", angle = -90, family = "Lato", color = "#F93822") +
  annotate("text", x = -.5, y = -35, label = "Negative", angle = -90, family = "Lato", color = "#39FF14") +
  annotate("text", x = 24, y = 92, label = "23: THE YULE BALL\nCan Dobby give Harry Potter his present?\nHe squeaked tentatively.", size = 4, family = "Lato", fontface = "italic", color = "#F93822", lineheight = .25, hjust = 0) +
  geom_curve(x = 26, y = 67, xend = 24, yend = 38, size = .05, color = "#F93822", curvature = -.5) +
  annotate("segment", x = 1, xend = 1, y = -250, yend = annotations$yend[1],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 2, xend = 2, y = -250, yend = annotations$yend[2],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 3, xend = 3, y = -250, yend = annotations$yend[3],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 4, xend = 4, y = -250, yend = annotations$yend[4],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 5, xend = 5, y = -250, yend = annotations$yend[5],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 6, xend = 6, y = -250, yend = annotations$yend[6],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 7, xend = 7, y = -250, yend = annotations$yend[7],
           colour = "#F93822", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 8, xend = 8, y = -250, yend = annotations$yend[8],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 9, xend = 9, y = -250, yend = annotations$yend[9],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 10, xend = 10, y = -250, yend = annotations$yend[10],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 11, xend = 11, y = -250, yend = annotations$yend[11],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 12, xend = 12, y = -250, yend = annotations$yend[12],
           colour = "#F93822", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 13, xend = 13, y = -250, yend = annotations$yend[13],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 14, xend = 14, y = -250, yend = annotations$yend[14],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 15, xend = 15, y = -250, yend = annotations$yend[15],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 16, xend = 16, y = -250, yend = annotations$yend[16],
           colour = "#F93822", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 17, xend = 17, y = -250, yend = annotations$yend[17],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 18, xend = 18, y = -250, yend = annotations$yend[18],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 19, xend = 19, y = -250, yend = annotations$yend[19],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 20, xend = 20, y = -250, yend = annotations$yend[20],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 21, xend = 21, y = -250, yend = annotations$yend[21],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 22, xend = 22, y = -250, yend = annotations$yend[22],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 23, xend = 23, y = -250, yend = annotations$yend[23],
           colour = "#F93822", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 24, xend = 24, y = -250, yend = annotations$yend[24],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 25, xend = 25, y = -250, yend = annotations$yend[25],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 26, xend = 26, y = -250, yend = annotations$yend[26],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 27, xend = 27, y = -250, yend = annotations$yend[27],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x =28, xend = 28, y = -250, yend = annotations$yend[28],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 29, xend = 29, y = -250, yend = annotations$yend[29],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 30, xend = 30, y = -250, yend = annotations$yend[30],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 31, xend = 31, y = -250, yend = annotations$yend[31],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 32, xend = 32, y = -250, yend = annotations$yend[32],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 33, xend = 33, y = -250, yend = annotations$yend[33],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 34, xend = 34, y = -250, yend = annotations$yend[34],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 35, xend = 35, y = -250, yend = annotations$yend[35],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 36, xend = 36, y = -250, yend = annotations$yend[36],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("segment", x = 37, xend = 37, y = -250, yend = annotations$yend[37],
           colour = "#39FF14", size = .15, linetype = "13", alpha = .5) +
  annotate("text", x = 30.8, y = -210, label = "35: VERITASERUM\nHe's back, Harry whispered.\nHe's back. Voldemort.", size = 4, family = "Lato", fontface = "italic", color = "#39FF14", lineheight = .25, hjust = 1) +
  geom_curve(x = 31, y = -195, xend = 34.5, yend = -230, size = .05, color = "#39FF14", curvature = -.5) +
  labs(x = "Chapter",
       y = "Aggregated Sentiment Score",
       title = "Harry Potter",
       subtitle = "                                  AND THE\n                                    GOBLET\n                                      OF FIRE",
       caption = "Visualization by Pablo Alvarez | Sentiment based on the AFINN lexicon") +
  theme_minimal(base_family = "Lato") +
  theme(panel.grid = element_blank(),
        plot.margin = margin(rep(10, 4)),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        axis.text.x = element_text(size = 12, color = axis_x_color, margin = margin(t = -7)),
        axis.text.y = element_text(size = 12, color = "white"),
        axis.title.x = element_text(size = 13, color = "white", hjust = .03, margin = margin(t = 5)),
        axis.title.y = element_text(angle = 90, color = "white", hjust = .8, size = 13),
        plot.title = element_text(color = "white", family = "harrypotter", size = 184),
        plot.subtitle = element_text(color = "white", family = "harrypotter", size = 32, lineheight = .25, margin = margin(t = -10, b = -20)),
        plot.caption = element_text(color = "white", hjust = .5, size = 12),
        legend.position = "none")

ggsave("30chartchallenge_21_down_upwards_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)

gg_playback()
