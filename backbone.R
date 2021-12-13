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
# library(patchwork)
# library(ggforce)
# library(ggtext)
# library(ggbump)
# library(ggalluvial)
# library(ggridges)
# library(ggfittext)
# library(ggupset)
# library(ggradar)
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
names(tuesdata)

# Data transformation


# Plot production




# Generate camcorder gif
gg_playback(
    name = here(paste0("week", week, "/camcorder/final.gif")),
    first_image_duration = 8,
    last_image_duration = 12,
    frame_duration = .25
)
