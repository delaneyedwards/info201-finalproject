setwd("C:\\Users\\delan\\Documents\\INFO201\\project-kellenmcgibbon\\data")
states_data <- read.csv("states_all.csv")
install.packages("tidyr")
library("tidyr")
install.packages("ggplot2") # once per machine
library("ggplot2")   
install.packages("maps")
library("maps")

recent_data <- states_data %>% 
  filter(YEAR == max(YEAR)) %>% 
  select(YEAR, STATE, AVG_READING_8_SCORE, AVG_MATH_8_SCORE)
  
  
