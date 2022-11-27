library(shiny)
library(tidyverse)

# create dummy data -------------------------------------------------------
set.seed(1)
substances <- c("L", "ketamine", "cocaine")
countries <- c("US", "Germany", "Namibia", "Denmark")
PS.data <- tibble(year = round(runif(90, year.range[1], year.range[2])),
                  country = countries[round(runif(90, 1, length(countries)))],
             substance = substances[round(runif(90, 1, length(substances)))])

PS.data
write_csv(PS.data, "PS.data.csv")


library(maps)
library(mapproj)
source("census-app/helpers.R")
counties <- readRDS("census-app/data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")

runApp("my_app", display.mode = "showcase")

library(rsconnect)
rsconnect::deployApp('test')  #PsyChild
