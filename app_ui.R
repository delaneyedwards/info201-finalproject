library(shiny)
library(plotly)

data_states <- read.csv("data/states_all.csv")
states_extended_data <- read.csv("data/states_all_extended.csv")
enrollment <- read.csv("data/Enrollment.csv")
gt <- read.csv("data/Gifted\ and\ Talented.csv")
ap <- read.csv("data/Advanced\ Placement.csv")
ib <- read.csv("data/International\ Baccalaureate.csv")

intro_page <- tabPanel(
  "Introduction Page"
)

bar_chart_page <- tabPanel(
  "Bar Chart",
  titlePanel("Average NAEP Math Scores by Race"),
) 

heatmap_page <- tabPanel(
  "Heatmap",
  titlePanel("Average Spendings and NAEP Scores By State")
)

stacked_bar_chart_page <- tabPanel(
  "Stacked Bar Chart",
  titlePanel("Distribution of Race Across Educational Programs")
)

ui <- navbarPage(
  "Factors that Affect Education ",
  intro_page,
  bar_chart_page,
  heatmap_page,
  stacked_bar_chart_page
)