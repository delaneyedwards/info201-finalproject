library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)

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

# Read CSV files
enrollment <- read.csv("data/Enrollment.csv")
gt <- read.csv("data/Gifted\ and\ Talented.csv")
ap <- read.csv("data/Advanced\ Placement.csv")
ib <- read.csv("data/International\ Baccalaureate.csv")

# Creates a data frame containing the number of students of each race enrolled
# in general enrollment.
enrollment_race <- enrollment %>%
  select(
    SCH_ENR_HI_M,
    SCH_ENR_HI_F,
    SCH_ENR_AM_M,
    SCH_ENR_AM_F,
    SCH_ENR_AS_M,
    SCH_ENR_AS_F,
    SCH_ENR_HP_M,
    SCH_ENR_HP_F,
    SCH_ENR_BL_M,
    SCH_ENR_BL_F,
    SCH_ENR_WH_M,
    SCH_ENR_WH_F,
    SCH_ENR_TR_M,
    SCH_ENR_TR_F
  ) %>%
  mutate(
    "Hispanic or Latino of any race" = SCH_ENR_HI_M + SCH_ENR_HI_F,
    .keep = "unused"
  ) %>%
  mutate(
    "American Indian or Alaska Native" = SCH_ENR_AM_M + SCH_ENR_AM_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Asian" = SCH_ENR_AS_M + SCH_ENR_AS_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Native Hawaiian or Other Pacific Islander" = SCH_ENR_HP_M + SCH_ENR_HP_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Black or African American" = SCH_ENR_BL_M + SCH_ENR_BL_F,
    .keep = "unused"
  ) %>%
  mutate(
    "White" = SCH_ENR_WH_M + SCH_ENR_WH_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Two or more races" = SCH_ENR_TR_M + SCH_ENR_TR_F,
    .keep = "unused"
  ) %>%
  gather(
    key = race,
    value = count,
  ) %>%
  filter(count >= 0) %>%
  group_by(race) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  mutate(program = "all_students")

# Creates a data frame containing the number of students of each race enrolled
# in Gifted and Talented programs.
gt_race <- gt %>%
  select(
    SCH_GTENR_HI_M,
    SCH_GTENR_HI_F,
    SCH_GTENR_AM_M,
    SCH_GTENR_AM_F,
    SCH_GTENR_AS_M,
    SCH_GTENR_AS_F,
    SCH_GTENR_HP_M,
    SCH_GTENR_HP_F,
    SCH_GTENR_BL_M,
    SCH_GTENR_BL_F,
    SCH_GTENR_WH_M,
    SCH_GTENR_WH_F,
    SCH_GTENR_TR_M,
    SCH_GTENR_TR_F
  ) %>%
  mutate(
    "Hispanic or Latino of any race" = SCH_GTENR_HI_M + SCH_GTENR_HI_F,
    .keep = "unused"
  ) %>%
  mutate(
    "American Indian or Alaska Native" = SCH_GTENR_AM_M + SCH_GTENR_AM_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Asian" = SCH_GTENR_AS_M + SCH_GTENR_AS_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Native Hawaiian or Other Pacific Islander" = SCH_GTENR_HP_M +
      SCH_GTENR_HP_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Black or African American" = SCH_GTENR_BL_M + SCH_GTENR_BL_F,
    .keep = "unused"
  ) %>%
  mutate(
    "White" = SCH_GTENR_WH_M + SCH_GTENR_WH_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Two or more races" = SCH_GTENR_TR_M + SCH_GTENR_TR_F,
    .keep = "unused"
  ) %>%
  gather(
    key = race,
    value = count,
  ) %>%
  filter(count >= 0) %>%
  group_by(race) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  mutate(program = "GT")

# Creates a data frame containing the number of students of each race enrolled
# in the Advanced Placement program.
ap_race <- ap %>%
  select(
    SCH_APENR_HI_M,
    SCH_APENR_HI_F,
    SCH_APENR_AM_M,
    SCH_APENR_AM_F,
    SCH_APENR_AS_M,
    SCH_APENR_AS_F,
    SCH_APENR_HP_M,
    SCH_APENR_HP_F,
    SCH_APENR_BL_M,
    SCH_APENR_BL_F,
    SCH_APENR_WH_M,
    SCH_APENR_WH_F,
    SCH_APENR_TR_M,
    SCH_APENR_TR_F
  ) %>%
  mutate(
    "Hispanic or Latino of any race" = SCH_APENR_HI_M + SCH_APENR_HI_F,
    .keep = "unused"
  ) %>%
  mutate(
    "American Indian or Alaska Native" = SCH_APENR_AM_M + SCH_APENR_AM_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Asian" = SCH_APENR_AS_M + SCH_APENR_AS_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Native Hawaiian or Other Pacific Islander" = SCH_APENR_HP_M +
      SCH_APENR_HP_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Black or African American" = SCH_APENR_BL_M + SCH_APENR_BL_F,
    .keep = "unused"
  ) %>%
  mutate(
    "White" = SCH_APENR_WH_M + SCH_APENR_WH_F, .keep = "unused"
  ) %>%
  mutate(
    "Two or more races" = SCH_APENR_TR_M + SCH_APENR_TR_F,
    .keep = "unused"
  ) %>%
  gather(
    key = race,
    value = count,
  ) %>%
  filter(count >= 0) %>%
  group_by(race) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  mutate(program = "AP")

# Creates a data frame containing the number of students of each race enrolled
# in the International Baccalaureate.
ib_race <- ib %>%
  select(
    SCH_IBENR_HI_M,
    SCH_IBENR_HI_F,
    SCH_IBENR_AM_M,
    SCH_IBENR_AM_F,
    SCH_IBENR_AS_M,
    SCH_IBENR_AS_F,
    SCH_IBENR_HP_M,
    SCH_IBENR_HP_F,
    SCH_IBENR_BL_M,
    SCH_IBENR_BL_F,
    SCH_IBENR_WH_M,
    SCH_IBENR_WH_F,
    SCH_IBENR_TR_M,
    SCH_IBENR_TR_F
  ) %>%
  mutate(
    "Hispanic or Latino of any race" = SCH_IBENR_HI_M + SCH_IBENR_HI_F,
    .keep = "unused"
  ) %>%
  mutate(
    "American Indian or Alaska Native" = SCH_IBENR_AM_M + SCH_IBENR_AM_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Asian" = SCH_IBENR_AS_M + SCH_IBENR_AS_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Native Hawaiian or Other Pacific Islander" = SCH_IBENR_HP_M +
      SCH_IBENR_HP_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Black or African American" = SCH_IBENR_BL_M + SCH_IBENR_BL_F,
    .keep = "unused"
  ) %>%
  mutate(
    "White" = SCH_IBENR_WH_M + SCH_IBENR_WH_F,
    .keep = "unused"
  ) %>%
  mutate(
    "Two or more races" = SCH_IBENR_TR_M + SCH_IBENR_TR_F,
    .keep = "unused"
  ) %>%
  gather(
    key = race,
    value = count,
  ) %>%
  filter(count >= 0) %>%
  group_by(race) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  mutate(program = "IB")


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
  
  output$distribution <- renderPlotly({
    # Combines all data frames into a single data frame.
    all_race <- rbind(enrollment_race, gt_race, ap_race, ib_race)
    
    data <- all_race %>% 
      filter(program == input$program)
    
    # Creates a stacked bar chart of all enrollment data.
    graph <- ggplot(data) +
      geom_col(
        mapping = aes(x = program, y = total, fill = race), position = "fill"
      ) +
      labs(
        x = "Program", y = "Proportion", fill = "Race",
        title = "Distribution of Race Across Educational Programs"
      )
    
    ggplotly(graph)
  })

}