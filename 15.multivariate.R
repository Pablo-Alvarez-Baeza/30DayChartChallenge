library(pacman)
p_load(tidyverse, spotifyr, showtext, ggtern)


# Fonts -------------------------------------------------------------------
font_add_google("Lato")
font_add(family = "Lato Bold",
         regular = "Lato-Bold.ttf")


# Spotify data ------------------------------------------------------------
# You need to enter your credentials. Read how here
# https://www.rcharlie.com/spotifyr/
Sys.setenv(SPOTIFY_CLIENT_ID = '08fd90e6cdf741288c317282d5fa1260')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a97a8bf8752742d59acb6eb11cdafada')


access_token <- get_spotify_access_token()

pinkfloyd_raw <- get_artist_audio_features('pink floyd')


# Data wrangling
pinkfloyd_dsotm <- pinkfloyd_raw |> 
  filter(album_name == "The Dark Side of the Moon")

pinkfloyd_dsotm |> 
  slice_max(valence, n = 1) |> 
  select(track_name)


# Plot --------------------------------------------------------------------
pinkfloyd_dsotm |> 
  ggplot(aes(x = valence, y = energy, z = instrumentalness, color = group, alpha = group, fill = "yellow")) +
  coord_tern() +
  geom_point(size = 1.5, color = "white", alpha = .5, show.legend = FALSE) +
  #geom_text(aes(label = track_name), size = 1, check_overlap = TRUE) +
  #geom_text(aes(label = energy), size = 1, check_overlap = TRUE) +
  labs(x = "Upbeat\ntunes  ", y = "Energetic", z = "Instrumental") +
  theme_noticks() +
  theme_hidegrid() +
  theme_hidelabels() +
  theme(plot.margin = margin(rep(10, 4)),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        axis.title = element_text(size = 4,
                                  color = "white",
                                  family = "Lato Bold",
                                  hjust = .5))


ggsave("pinfloyd_dsotm.png",  width = 3.375, height = 3.375, units = "in", dpi = 320)
