# Load main libraries
library(tidyverse)
library(fs)
library(here)
library(showtext)
library(camcorder)
library(hrbrthemes)
library(wesanderson)

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
indoor <- tuesdata$indoor_pollution
pop <- tuesdata$fuel_gdp %>%
    select(Entity, Code, Year, `Population (historical estimates)`)

# Data transformation
names(indoor) <- c(names(indoor)[1:3], "values")

# Plot production
indoor %>% 
    mutate(values_old = values,
           values = round(values_old, 0))  %>% 
#     filter(grepl(c("Western|South|East|North"), Entity)) %>%
    filter(grepl(c("Europe"), Entity)) %>%
    ggplot(aes(Year, Entity, fill = as.factor(values))) +
    geom_tile(color = "black") 

indoor  %>% 
#     filter(Entity == "European Union")  %>% 
    filter(grepl("European", Entity))  %>% 
    group_by(Entity)  %>% 
    ggplot(aes(Year, values)) +
    geom_line() +
    geom_point(data = . %>% filter(values == max(values)),
               aes(Year, values), color = "red") +
    geom_point(data = . %>% filter(values == min(values)),
               aes(Year, values), color = "blue") +
    facet_wrap(vars(Entity))

indoor  %>% 
    left_join(pop)  %>% 
#     filter(Entity == "European Union")  %>% 
    filter(grepl("Italy|France|Spain|Portugal|Germany|Kingdom", Entity))  %>% 
    group_by(Entity)  %>% 
    ggplot(aes(Year, values, color = Entity)) +
    geom_point(aes(size = `Population (historical estimates)`),
               alpha = .6) +
    geom_line(size = 1) +
#     geom_point(data = . %>% filter(values == max(values)),
#                aes(Year, values), color = "red") +
#     geom_point(data = . %>% filter(values == min(values)),
#                aes(Year, values), color = "blue") +
#    scale_color_manual(values = wes_palette("GrandBudapest1", n = 9)) +
   theme_ft_rc()


# Generate camcorder gif
gg_playback(
    name = here(paste0("2021_week", week, "/camcorder/final.gif")),
    first_image_duration = 8,
    last_image_duration = 12,
    frame_duration = .25
)
