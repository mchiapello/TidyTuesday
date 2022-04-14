# Which week do you want to analyze?
day <- '2022-04-12'
year <- 2022
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
