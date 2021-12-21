# Load main libraries
library(tidyverse)
library(fs)
library(here)
library(showtext)
library(camcorder)

# Load additional 
# library(ggfx)
# library(ggrepel)
# library(ggforce)
# library(wesanderson)
library(patchwork)
# library(ggforce)
# library(ggtext)
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
    dir = here(paste0("2021_week", week, "/camcorder")),
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
names(tuesdata)
st <- tuesdata[[1]]

# Data transformation
fil <- st %>% 
    select(product_name, size) %>% 
    distinct() %>% 
    count(product_name) %>% 
    filter(n == 4)

st2 <- st %>% 
    select(-milk, -whip, -sodium_mg, -serv_size_m_l) %>% 
    mutate(trans_fat_g = as.numeric(trans_fat_g),
           fiber_g = as.numeric(fiber_g)) %>% 
    filter(product_name %in% fil$product_name) %>% 
    group_by(product_name) %>% 
    nest()

x <- st2$data[[1]]

x2 <- x %>% 
    mutate(across(where(is.numeric), ~ .x / sum(.x + 1))) %>% 
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)),
           size = "marco") %>%
    mutate(across(where(is.numeric), sum)) %>% 
    distinct

    

# Plot production
x2 %>% 
    ggradar(
    font.radar = "m300",
    grid.label.size = 0,  # Affects the grid annotations (0%, 50%, etc.)
    axis.label.size = 2.5, # Afftects the names of the variables
    group.point.size = 3   # Simply the size of the point 
) 

plotgg <- function(x){
    x2 %>% 
        pivot_longer(cols = -size,
                     names_to = "feat",
                     values_to = "value") %>% 
        ggplot(aes(feat, value, fill = feat)) +
        geom_col() +
        coord_polar() +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.title = element_blank(),
              legend.position = "none")
}
x2 %>% 
    pivot_longer(cols = -size,
                 names_to = "feat",
                 values_to = "value") %>% 
    ggplot(aes(feat, value, group = size)) +
    geom_jitter() +
    geom_line() +
    coord_polar()




ggsave(paste0("2021_week", week, "/final.png"), units = "mm",
       width = 210, height = 297, dpi = 600) 

# Generate camcorder gif
gg_playback(
    name = here(paste0("2021_week", week, "/camcorder/final.gif")),
    first_image_duration = 8,
    last_image_duration = 12,
    frame_duration = .25
)
