# Which week do you want to analyze?
day <- '2021-12-07'
year <- 2021
week <- lubridate::week(lubridate::ymd(day))    

# Create the analysis folder
if(!dir.exists(here::here(paste0(year, "_week", week)))){
    fs::dir_create(paste0(year, "_week", week))
} else {
    message("This week folder already exist!")
}

# Load data
tuesdata <- tidytuesdayR::tt_load(day)

# Create backbone script
if (!file.exists(paste0(year, "_week", week, "/", day, ".R"))){
    fs::file_copy("backbone.R", paste0(year, "_week", week, "/", day, ".R"))
} else {
    message("This file already exist!")
}
