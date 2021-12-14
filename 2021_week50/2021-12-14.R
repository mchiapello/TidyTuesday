# Load main libraries
library(tidyverse)
library(fs)
library(here)
library(showtext)
library(camcorder)

# Load additional 
library(tidytext)
library(ggfx)
# library(ggrepel)
# library(ggforce)
# library(wesanderson)
library(patchwork)
# library(ggforce)
library(ggtext)
# library(ggbump)
# library(ggalluvial)
# library(ggridges)
# library(ggfittext)
# library(ggupset)
library(ggradar)
# library(ggworcloud)
# library(ggcorrplot)
# library(ggnetwork)
# library(ggbreak)
# library(ggmuller)
# library(ggQC)
# library(ggheatmap)


# Load the needed fonts
font_add_google("Montserrat", "m300", regular.wt = 300)
font_add_google("Montserrat", "m600", regular.wt = 600)
showtext_auto()

# Set up camcorder
gg_record(
    dir = here(paste0("week", week, "/camcorder")),
    device = "png", 
    units = "cm", 
    dpi = 300 
)

# gg_resize_film(
#     height = 4,
#     width = 6,
#     units = "cm",
#     dpi = 350
# )

# Collect data
studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')
lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')


# Data transformation
## Prepare lyrics
sent <- lyrics %>%
    unnest_tokens(word, line) %>%
    inner_join(bing) %>%
    count(word, sentiment, sort = TRUE)

## Prepare feature
df <- studio_album_tracks %>%
    group_by(album_name) %>%
    summarise(
        danceability_mean = mean(danceability),
        energy_mean = mean(energy),
        valence_mean = mean(valence),
        acousticness_mean = mean(acousticness),
        speechiness_mean = mean(speechiness)) %>%
    ungroup() %>%
    mutate(
        album_name = factor(
            album_name, levels = c("Spice", "Spiceworld", "Forever"))) %>%
    arrange(album_name) %>% 
    pivot_longer(cols = -album_name,
                 names_to = "feature",
                 values_to = "value") %>% 
    group_by(feature) %>% 
    mutate(tot = sum(value),
           new = value / tot ) %>% 
    pivot_wider(id_cols = c("album_name"),
                names_from = feature,
                values_from = new)

# Spice girl image
img <- jpeg::readJPEG("2021_week50/img.jpg", native = TRUE)

# Plot production
p1 <- sent %>%
    filter(n > 10) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n),
           word = fct_reorder(word, n)) %>%
    arrange(word) %>% 
    mutate(y = 0,
        x1 = seq(0.55, 23.55, 1),
        x2 = seq(1.45, 24.45, 1),
        y1 = ifelse(n < 0, -5, 5),
        y2 = ifelse(n < 0, n - 5, n + 5),
        ord = row_number()) %>% 
    ggplot(aes(ord, n, fill = sentiment, label = word)) +
    with_shadow(
        geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2)),
        sigma = 1,
        colour = "#ece2f0") +
    coord_flip() +
    geom_text(aes(ord, y), family = "m600", size = 12) +
    scale_fill_manual(values = c("#f46d43", "#66bd63")) +
    annotate("text", x = 15, y = 50, hjust = 1, 
             size = 15, family = "m300", lineheight = .3,
             label = "Positive\nsentiments",
             ) +
    annotate("text", x = 5, y = -25, hjust = 1, 
             size = 15, family = "m300", lineheight = .3,
             label = "Negative\nsentiments",
    ) +
    labs(title = "Sentiments of Spice Girls songs") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(family = "m600", size = 100, hjust = .5),
          plot.subtitle = element_markdown(family = "m300", size = 50)) +
    NULL

p2 <- df %>% 
    rename(`Danceability` = danceability_mean,
           `Energy` = energy_mean,
           `Valence` = valence_mean,
           `Acousticness` = acousticness_mean,
           `Speechiness` = speechiness_mean) %>% 
    ggradar(
    font.radar = "m300",
    grid.label.size = 5,
    axis.label.size = 10,
    group.point.size = .5) +
    labs(title = "Feature per album",
         subtitle = "Albums: 
         <span style = 'color:#377eb8;'>Spice</span>, 
         <span style = 'color:#4daf4a;'>Spiceworld</span>, 
         <span style = 'color:#ff7f00;'>Forever</span>") +
    theme(legend.position = "none",
          legend.text = element_text(family = "m300", size = 25),
          plot.title = element_text(family = "m600", size = 70, hjust = .5),
          plot.subtitle = element_markdown(family = "m300", size = 50, hjust = .5)) +
    scale_color_manual(values = c("#377eb8", "#4daf4a", "#ff7f00"))


p1 + inset_element(p2, 
                   left = .6, 
                   bottom = .3,
                   right = 1,
                   top = .9) +
    inset_element(img, 
                  left = .4, 
                  bottom = 0.05, 
                  right = .8, 
                  top = .35)

ggsave(paste0("2021_week", week, "/figure.jpeg"), units = "mm", width = 297, height = 210, scale = 1.1)

# Generate camcorder gif
gg_playback(
    name = here(paste0("week", week, "/camcorder/final.gif")),
    first_image_duration = 8,
    last_image_duration = 12,
    frame_duration = .25
)
