# Load main libraries
library(tidyverse)
library(fs)
library(here)
library(showtext)
library(wesanderson)
library(patchwork)

# Load the needed fonts
font_add_google("Montserrat", "m300", regular.wt = 300)
font_add_google("Montserrat", "m600", regular.wt = 600)
font_add_google("Nanum Pen Script", "n400")
showtext_auto()

# Collect data
day <- '2021-12-21'
tuesdata <- tidytuesdayR::tt_load(day)
st <- tuesdata[[1]]

# Data transformation
fil <- st %>% 
    select(product_name, size) %>% 
    distinct() %>% 
    count(product_name) %>% 
    filter(n == 4)

# Nest data by product_name
st2 <- st %>% 
    select(-milk, -whip, -sodium_mg, -serv_size_m_l) %>% 
    mutate(trans_fat_g = as.numeric(trans_fat_g),
           fiber_g = as.numeric(fiber_g)) %>% 
    filter(product_name %in% fil$product_name) %>% 
    group_by(product_name) %>% 
    nest()

# Compute Number of feature per product_name
st3 <- st2 %>% 
    mutate(NN = map(data, ~ .x %>% 
                        mutate(across(where(is.numeric), ~ .x / sum(.x + 1))) %>% 
                        mutate(across(where(is.numeric), ~ replace_na(.x, 0)),
                               size = "marco") %>%
                        mutate(across(where(is.numeric), sum)) %>% 
                        distinct %>% 
                        mutate(across(where(is.numeric), ~if_else(. == 0, 0, 1))) %>% 
                        rowwise() %>% 
                        mutate(sumVar = sum(c_across(calories:caffeine_mg))) %>% 
                        pull(sumVar))) %>% 
    unnest(NN) %>% 
    arrange(desc(NN)) %>% 
    mutate(product_name = factor(product_name)) 


# Plot function
plotgg <- function(x, tit = product_name){
    pal <- wes_palette("Royal1", 10, type = "continuous")
    out <- x %>% 
        mutate(across(where(is.numeric), ~ .x / sum(.x + 1))) %>% 
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)),
               size = "marco") %>%
        mutate(across(where(is.numeric), sum)) %>% 
        distinct %>% 
        pivot_longer(cols = -size,
                     names_to = "feat",
                     values_to = "value") %>% 
        ggplot(aes(feat, value, fill = feat)) +
        geom_col(color = "black", size = .1) +
        coord_polar() +
        labs(title = tit) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              # axis.text.x = element_text(family = "m600"),
              axis.title = element_blank(),
              legend.position = "none",
              plot.title = element_text(family = "m600", size = 20)) +
        scale_fill_manual(values = pal)
}

# Compute the plor
st4 <- st3 %>% 
    mutate(plot = map(data, ~plotgg(.x, tit = product_name)))

# Create plot wrap
p1 <- wrap_plots(st4$plot)

# Create legend
p2 <- st3$data[[1]] %>% 
    slice(1) %>% 
    mutate(across(where(is.numeric), ~ .5)) %>% 
    pivot_longer(cols = -size,
                 names_to = "feat",
                 values_to = "value") %>% 
    mutate(feat = case_when(feat == "calories" ~ "Calories",
                            feat == "total_fat_g" ~ "Total\nfat",
                            feat == "saturated_fat_g"~ "Saturated\nfat",
                            feat == "trans_fat_g" ~ "Trans\nfat",
                            feat == "cholesterol_mg" ~ "Cholesterol",
                            feat == "total_carbs_g" ~ "Total\ncarbs",
                            feat == "fiber_g" ~ "Fiber",
                            feat == "sugar_g" ~ "Sugar",
                            feat == "caffeine_mg" ~ "Caffeine")) %>% 
    ggplot(aes(feat, value, fill = feat)) +
    geom_col(color = "black", size = .1) +
    ylim(c(0, .7)) +
    labs(caption = "Everithing is expressed in grams, apart for caffeine and Chlolesterol") +
    # scale_y_continuous(expand = c(0.2, 0)) +
    coord_polar() +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.grid.major.x = element_line(color = "#D4C7C5"),
          axis.text = element_blank(),
          axis.text.x = element_text(family = "m600", lineheight = 0.2, size = 35),
          axis.title = element_blank(),
          legend.position = "none",
          plot.caption = element_text(lineheight = 0.1, size = 30),
          plot.title = element_text(family = "m600", size = 20)) +
    scale_fill_manual(values = pal)

# Create title
p3 <- tibble(x = 1:5, y = 1:5) %>% 
    ggplot(aes(x, y)) +
    annotate("text", x = 0, y = 2, label = "Analysis of Starbucks\'\nbeverage components",
             family = "n400", size = 100, lineheight = .2) +
    theme_void() 

# Create final plot
layout <- "
AAAAAABBB
AAAAAABBB
CCCCCCCCC
CCCCCCCCC
CCCCCCCCC
CCCCCCCCC
CCCCCCCCC
CCCCCCCCC
CCCCCCCCC
CCCCCCCCC
CCCCCCCCC
"

p3 + p2 + p1 +
    plot_layout(design = layout)

# save it
ggsave(paste0("2021_week", week, "/final.png"), units = "mm",
       width = 210, height = 297, dpi = 600) 

