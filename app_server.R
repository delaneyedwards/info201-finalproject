library(shiny)
library(ggplot2)
library(dplyr)

data_states <- read.csv("data/states_all.csv")
states_extended_data <- read.csv("data/states_all_extended.csv")
enrollment <- read.csv("data/Enrollment.csv")
gt <- read.csv("data/Gifted\ and\ Talented.csv")
ap <- read.csv("data/Advanced\ Placement.csv")
ib <- read.csv("data/International\ Baccalaureate.csv")

server <- function(input, output) {
  
}