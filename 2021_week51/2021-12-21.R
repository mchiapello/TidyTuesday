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
st %>% count(caffeine_mg, size, sort = TRUE)

stt <- st %>% 
    mutate(trans_fat_g = as.numeric(trans_fat_g),
           fiber_g = as.numeric(fiber_g)) %>% 
    dplyr::select(product_name, size, calories, total_fat_g, cholesterol_mg,
                  total_carbs_g:caffeine_mg) %>% 
    group_by(product_name) %>% 
    summarise(across(where(is.numeric), list(mean = mean, sd = sd))) %>% 
    mutate(class = case_when(grepl("Tea", product_name,
                                   ignore.case = TRUE) ~ "Tea",
                             grepl("Caffè", product_name,
                                   ignore.case = TRUE) ~ "Caffè",
                             grepl("Frappuccino", product_name,
                                   ignore.case = TRUE) ~ "Frappuccino",
                             grepl("coffee", product_name, 
                                   ignore.case = TRUE) ~ "Caffè",
                             grepl("chocolate", product_name, 
                                   ignore.case = TRUE) ~ "Chocolate",
                             grepl("Espresso", product_name, 
                                   ignore.case = TRUE) ~ "Espresso",
                             grepl("Latte", product_name, 
                                   ignore.case = TRUE) ~ "Latte",
                             TRUE ~ "Other"),
           class = fct_infreq(factor(class))) %>% 
    relocate(class, .after = product_name) 


plotgg <- function(x, tit = class){
    x %>% 
        ggplot(aes(product_name, total_fat_g_mean)) +
            geom_errorbar(aes(ymin = total_fat_g_mean - total_fat_g_sd,
                              ymax = total_fat_g_mean + total_fat_g_sd),
                          size = 0.1, width = 0.25) +
            geom_point(color = "#F5882F", size = 1) +
        geom_errorbar(data = x,
                      aes(ymin = calories_mean - calories_sd,
                          ymax = calories_mean + calories_sd),
                      size = 0.1, width = 0.25) +
        geom_point(data = x,
                   aes(product_name, calories_mean), 
                   color = "blue", size = 1) +
        geom_errorbar(data = x,
                      aes(ymin = cholesterol_mg_mean - cholesterol_mg_sd,
                          ymax = cholesterol_mg_mean + cholesterol_mg_sd),
                      size = 0.1, width = 0.25) +
        geom_point(data = x,
                   aes(product_name, cholesterol_mg_mean), 
                   color = "green", size = 1) +
        geom_errorbar(data = x,
                      aes(ymin = sugar_g_mean - sugar_g_sd,
                          ymax = sugar_g_mean + sugar_g_sd),
                      size = 0.1, width = 0.25) +
        geom_point(data = x,
                   aes(product_name, sugar_g_mean), 
                   color = "brown", size = 1) +
        geom_errorbar(data = x,
                      aes(ymin = caffeine_mg_mean - caffeine_mg_sd,
                          ymax = caffeine_mg_mean + caffeine_mg_sd),
                      size = 0.1, width = 0.25) +
        geom_point(data = x,
                   aes(product_name, caffeine_mg_mean), 
                   color = "purple", size = 1) +
        labs(x = "",
             y = "",
             title = tit) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.text = element_text(family = "m300", size = 35),
              title = element_text(family = "m600", size = 15,
                                   hjust = .5)) +
        scale_y_log10() +
        coord_flip()
}


# Plot production
df <- stt %>% 
    group_by(class) %>% 
    nest() %>% 
    mutate(N = map_int(data, nrow)) %>% 
    arrange(desc(N)) %>% 
    mutate(plot = map(data, ~plotgg(.x, tit = class)))



# stt %>% 
#     ggplot(aes(product_name, total_fat_g_mean)) +
#     geom_errorbar(aes(ymin = total_fat_g_mean - total_fat_g_sd,
#                       ymax = total_fat_g_mean + total_fat_g_sd),
#                   size = 0.1, width = 0.25) +
#     geom_point(color = "red") +
#     geom_errorbar(data = stt,
#                   aes(ymin = calories_mean - calories_sd,
#                       ymax = calories_mean + calories_sd),
#                   size = 0.1, width = 0.25) +
#     geom_point(data = stt,
#                aes(product_name, calories_mean), color = "blue") +
#     geom_errorbar(data = stt,
#                   aes(ymin = cholesterol_mg_mean - cholesterol_mg_sd,
#                       ymax = cholesterol_mg_mean + cholesterol_mg_sd),
#                   size = 0.1, width = 0.25) +
#     geom_point(data = stt,
#                aes(product_name, cholesterol_mg_mean), color = "green") +
#     geom_errorbar(data = stt,
#                   aes(ymin = sugar_g_mean - sugar_g_sd,
#                       ymax = sugar_g_mean + sugar_g_sd),
#                   size = 0.1, width = 0.25) +
#     geom_point(data = stt,
#                aes(product_name, sugar_g_mean), color = "brown") +
#     geom_errorbar(data = stt,
#                   aes(ymin = caffeine_mg_mean - caffeine_mg_sd,
#                       ymax = caffeine_mg_mean + caffeine_mg_sd),
#                   size = 0.1, width = 0.25) +
#     geom_point(data = stt,
#                aes(product_name, caffeine_mg_mean), color = "purple") +
#     coord_flip() +
#     facet_wrap(vars(class))

ggsave(paste0("2021_week", week, "/final.png"), units = "mm",
       width = 210, height = 297, dpi = 600) 

# Generate camcorder gif
gg_playback(
    name = here(paste0("2021_week", week, "/camcorder/final.gif")),
    first_image_duration = 8,
    last_image_duration = 12,
    frame_duration = .25
)
