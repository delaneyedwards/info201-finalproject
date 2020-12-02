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
  titlePanel("Average Spendings and NAEP Scores By State"),
  sidebarLayout(
    sidebarPanel(
      h4("Manipulate the values below and view different data
               on the maps to the right."),
      fill <- selectInput(
        "fill",
        label = "Choose the Grade You Want to Evaluate",
        choices = list(
          "4th Grade" = "grade_4",
          "8th Grade" = "grade_8"
        ),
        selected = "grade_4"
      ),
      color_high <- selectInput(
        "color_high",
        label = "Choose the High Color You Would Like to View the Maps In",
        choices = list(
          "Red" = "red",
          "Orange" = "orange",
          "Yellow" = "yellow",
          "Blue" = "blue",
          "Green" = "green",
          "Purple" = "purple",
          "White" = "white",
          "Black" = "black"
        ),
        selected = "red"
      ),
      color_low <- selectInput(
        "color_low",
        label = "Choose the Low Color You Would Like to View the Maps In",
        choices = list(
          "Red" = "red",
          "Orange" = "orange",
          "Yellow" = "yellow",
          "Blue" = "blue",
          "Green" = "green",
          "Purple" = "purple",
          "White" = "white",
          "Black" = "black"
        ),
        selected = "yellow"
      )
    ),
    mainPanel(
      plotlyOutput("moneymap"),
      plotlyOutput("scores")
    )
  )
)

stacked_bar_chart_page <- tabPanel(
  "Stacked Bar Chart",
  titlePanel("Distribution of Race Across Educational Programs")
)

summary_page <- tabPanel(
  "Summary Takeaways"
)

ui <- navbarPage(
  "Factors that Affect Education ",
  intro_page,
  bar_chart_page,
  heatmap_page,
  stacked_bar_chart_page,
  summary_page
)