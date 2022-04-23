library(pacman)
p_load(tidyverse, showtext)


# Fonts -------------------------------------------------------------------
font_add(family = "Merriweather Black",
         regular = "Merriweather-Black.ttf")

font_add(family = "Georgia",
         regular = "Georgia.ttf")

font_add_google("Lato")


# Data --------------------------------------------------------------------
# https://www.forbes.com/billionaires/
df <- data.frame(
  net_worth = c(219, 171, 158, 129, 118, 111, 107, 106,
                91.4, 90.7),
  name = c("Elon Musk\n219B", "Jeff Bezos\n171B", "Bernard Arnault\n& family\n158B",
           "Bill Gates\n129B", "Warren Buffet\n118B", "Larry Page\n111B",
           "Sergey Brin\n107B", "Larry Ellison\n106B", "Steve Ballmer\n91.4B", "Mukesh\nAmbani\n90B")
)


# Plot --------------------------------------------------------------------
df |> 
  ggplot(aes(name, net_worth)) +
  geom_col(aes(fct_reorder(name, -net_worth)), width = 1, color = "black", size = .1, fill = "#669966") +
  geom_text(aes(label = name), size = 1.25, vjust = -.5, family = "Lato") +
  scale_y_continuous(limits = c(0, 230)) +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  labs(title = "The Richest People in the World",
       caption = "Visualization by Pablo Alvarez | Data from Forbes World's Billionaires List 2022") +
  theme_void() +
  theme(plot.margin = margin(rep(10, 4)),
        panel.background = element_rect(fill = "grey75", color = "grey75"),
        plot.background = element_rect(fill = "grey75", color = "grey75"),
        plot.title = element_text(family = "Merriweather Black",
                                  size = 14,
                                  margin = margin(b = 80)),
        plot.caption = element_text(family = "Lato",
                                    size = 4,
                                    hjust = 0,
                                    margin = margin(t = 10)))

ggsave("30chartchallenge_9_statistics_2022.png", width = 1080, height = 1080, units = "px", dpi = 320)

           