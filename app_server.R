library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

data_states <- read.csv("data/states_all.csv")
states_extended_data <- read.csv("data/states_all_extended.csv")
enrollment <- read.csv("data/Enrollment.csv")
gt <- read.csv("data/Gifted\ and\ Talented.csv")
ap <- read.csv("data/Advanced\ Placement.csv")
ib <- read.csv("data/International\ Baccalaureate.csv")

after_2000_refined <- data_states %>%
  filter(YEAR > 2011 & YEAR < 2016) %>%
  select(
    STATE,
    INSTRUCTION_EXPENDITURE,
    SUPPORT_SERVICES_EXPENDITURE,
    ENROLL
  ) %>%
  group_by(STATE) %>%
  summarize(
    mean_inst_exp = mean(INSTRUCTION_EXPENDITURE, na.rm = TRUE),
    mean_ss_exp = mean(SUPPORT_SERVICES_EXPENDITURE, na.rm = TRUE),
    mean_enrolled = mean(ENROLL, na.rm = TRUE)
  ) %>%
  group_by(STATE) %>%
  mutate(expenditure = sum(mean_inst_exp + mean_ss_exp, na.rm = TRUE)) %>%
  mutate(exp_per_student = expenditure / mean_enrolled) %>%
  rename(state = STATE)


after_2000_refined$state <- tolower(after_2000_refined$state)
after_2000_refined$state <- gsub("_", " ", after_2000_refined$state)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(after_2000_refined, by = "state")

# this data set will create a table with the average test scores for 4th
# and 8th graders per state for the years 2012 - 2015
math_scores_refined <- data_states %>%
  filter(YEAR > 2011 & YEAR < 2016) %>%
  select(STATE, AVG_MATH_4_SCORE:AVG_READING_8_SCORE) %>%
  group_by(STATE) %>%
  summarize(
    mean_math_4 = mean(AVG_MATH_4_SCORE, na.rm = TRUE),
    mean_math_8 = mean(AVG_MATH_8_SCORE, na.rm = TRUE),
    mean_reading_4 = mean(AVG_READING_8_SCORE, na.rm = TRUE),
    mean_reading_8 = mean(AVG_READING_8_SCORE, na.rm = TRUE)
  ) %>%
  group_by(STATE) %>%
  mutate(
    grade_4 = mean(mean_math_4 + mean_reading_4, na.rm = TRUE),
    grade_8 = mean(mean_math_8 + mean_reading_8, na.rm = TRUE)
  ) %>%
  rename(state = STATE)

math_scores_refined$state <- tolower(math_scores_refined$state)
math_scores_refined$state <- gsub("_", " ", math_scores_refined$state)

state_shape_grade <- map_data("state") %>%
  rename(state = region) %>%
  left_join(math_scores_refined, by = "state")

translate <- list(
  "grade_4" = "4th Grade",
  "grade_8" = "8th Grade"
)

server <- function(input, output) {
  # this creates a heatmap of the Money Spent on Education per child for each
  # state from the years 2012 to 2015
  output$moneymap <- renderPlotly({
    title <- paste0("Average Amount of Money Spent Per Student by State")
    p <- ggplot(state_shape) +
      geom_polygon(
        mapping = aes_string(x = "long", y = "lat", group = "group", fill = "exp_per_student"),
        color = "white",
        size = .1
      ) +
      coord_map() +
      scale_fill_continuous(low = input$color_low, high = input$color_high) +
      labs(fill = "Dollars per Child", title = title) +
      blank_theme
    p
  })
  # this creates a heatmap of the Average Grade Score per student in grade for
  # each state from the years 2012 to 2015
  output$scores <- renderPlotly({
    title <- paste0("Average Scores by State: ", translate[[input$fill]])
    p <- ggplot(state_shape_grade) +
      geom_polygon(
        mapping = aes_string(x = "long", y = "lat", group = "group", fill = input$fill),
        color = "white",
        size = .1
      ) +
      coord_map() +
      scale_fill_continuous(low = input$color_low, high = input$color_high) +
      labs(fill = translate[[input$fill]], title = title) +
      blank_theme 
    p
  })
}
