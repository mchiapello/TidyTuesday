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
#library(ggalluvial)
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
font_add_google("Shalimar", "s400")
font_add_google("Architects Daughter", "ad400")
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
spiders <- tuesdata$spider

# Data transformation
top <- spiders %>% 
    count(family, sort = TRUE) %>% 
    slice(1:10)

df <- spiders %>% 
    filter(family %in% top$family) %>% 
    mutate(family = factor(family, levels = top$family))
   
# Plot production
df %>% 
    count(family, species, year) %>% 
    group_by(family) %>% 
    slice_max(n)

df1 <- df %>% 
    count(family, year, sort = TRUE) %>% 
    group_by(family) %>% 
    mutate(M = mean(n),
           SD = sd(n),
           upper = M + SD) %>% 
    filter(n > SD) %>% 
    mutate(col = ifelse(n == max(n), "red", "black")) %>% 
    ungroup() 

df1 %>% 
    ggplot(aes(x = year, y = family, size = n)) +
    geom_point(alpha = .4) +
    geom_point(data = df1 %>% 
                   mutate(col = ifelse(col == "black", NA, col)),
               aes(x = year, y = family, size = n, color = col)) +
    geom_point(data = df %>% 
                   count(family, genus, year, sort = TRUE) %>% 
                   group_by(family) %>% 
                   slice_max(n) %>% 
                   mutate(col = "blue"),
               aes(x = year, y = family, color = col, size = n)) +
    # geom_point(data = tibble(year = 2030, family = "Salticidae", col = NA, n = 500),
    #            aes(x = year, y = family, color = col, size = n)) +
    scale_color_identity() +
    # theme_void() +
    scale_x_continuous(expand = c(0.05, 0)) +
    scale_y_discrete(expand = c(0.1, .1)) +
    theme_void() +
    theme(axis.text.x.bottom = element_text(size = 80, family = "m600"),
          axis.text.y.left = element_text(size = 90, family = "ad400", hjust = 1,
                                          vjust = 0),
          legend.position = "none",
          panel.grid.major.x = element_line(color = "grey90"),
          plot.title = element_text(family = "ad400", size = 110, hjust = .5),
          plot.background = element_rect(color = "white")) +
    labs(title = "") +
    annotate(geom = "curve", x = 1900, y = 7.5, xend = 1906.5, yend = 7.15,
             curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x = 1898, y = 7.5, 
             label = "Maximum number of\ndifferent genus recored", 
             hjust = 1, size = 10, family = "m300",
             lineheight = .3) +
    annotate(geom = "curve", x = 1935.6, y = 7.5, xend = 1929, yend = 7.15,
             curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
annotate(geom = "text", x = 1936.5, y = 7.5, 
             label = "Maximum number of\ndifferent family recored", 
             hjust = "left", size = 10, family = "m300",
             lineheight = .3) +
    annotate(geom = "curve", x = 1935, y = 10.5, xend = 1940, yend = 10.15,
             curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x = 1934, y = 10.5, 
             label = "Number of different spider \nrecored over the mean value", 
             hjust = 1, size = 10, family = "m300",
             lineheight = .3) +
    labs(title = "Spiders identified in the last two centuries",
         caption = "@marpello1980")

ggplot2::ggsave("week49/final.png", units = "cm", width = 29.7, height = 21, scale = 1.05)

# Generate camcorder gif
gg_playback(
    name = here(paste0("week", week, "/camcorder/final.gif")),
    first_image_duration = 8,
    last_image_duration = 12,
    frame_duration = .25
)
