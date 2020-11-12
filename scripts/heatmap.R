library("tidyverse")
library("ggplot2")
library("mapproj")

#setwd("C:/Users/sahit/Desktop/info201-groupproject")
data_states <- read.csv("data/states_all.csv")

# this data set will create a table with the means of the expenditures, students
# enrolled, and expenditure per state for the years 2012 - 2015
after_2000_refined <- data_states %>% 
  filter(YEAR > 2011 & YEAR < 2016) %>% 
  select(STATE, 
         INSTRUCTION_EXPENDITURE, 
         SUPPORT_SERVICES_EXPENDITURE, 
         ENROLL) %>% 
  group_by(STATE) %>% 
  summarize(mean_inst_exp = mean(INSTRUCTION_EXPENDITURE, na.rm = TRUE),
            mean_ss_exp = mean(SUPPORT_SERVICES_EXPENDITURE, na.rm = TRUE),
            mean_enrolled = mean(ENROLL, na.rm = TRUE)) %>%
  group_by(STATE) %>% 
  mutate(expenditure = sum(mean_inst_exp + mean_ss_exp, na.rm = TRUE)) %>% 
  mutate(exp_per_student = expenditure/mean_enrolled)

after_2000_refined$STATE <- tolower(after_2000_refined$STATE)
after_2000_refined$STATE <- gsub('_', ' ', after_2000_refined$STATE)

library("maps")

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
  rename(STATE = region) %>% 
  left_join(after_2000_refined, by = "STATE")

# this creates a heatmap of the Money Spent on Education for each state from 
# the years 2012 to 2015
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = expenditure),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "Yellow",
                        high = "Red",
                        limits = c(800000, 58000000)) +
  labs(fill = "Money Spent on Education") +
  blank_theme

# this creates a heatmap of the Money Spent on Education per child for each 
# state from the years 2012 to 2015
spent_per_child <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = exp_per_student),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "Yellow", high = "Red") +
  labs(fill = "Average amount of money per Child") +
  blank_theme

# this data set will create a table with the average test scores for 4th 
# and 8th graders per state for the years 2012 - 2015
math_scores_refined <- data_states %>% 
  filter(YEAR > 2011 & YEAR < 2016) %>% 
  select(STATE, AVG_MATH_4_SCORE:AVG_READING_8_SCORE) %>% 
  group_by(STATE) %>% 
  summarize(mean_math_4 = mean(AVG_MATH_4_SCORE, na.rm = TRUE),
            mean_math_8 = mean(AVG_MATH_8_SCORE, na.rm = TRUE),
            mean_reading_4 = mean(AVG_READING_8_SCORE, na.rm = TRUE),
            mean_reading_8 = mean(AVG_READING_8_SCORE, na.rm = TRUE)) %>%
  group_by(STATE) %>% 
  mutate(grade_4 = mean(mean_math_4 + mean_reading_4, na.rm = TRUE),
         grade_8 = mean(mean_math_8 + mean_reading_8, na.rm = TRUE))

math_scores_refined$STATE <- tolower(math_scores_refined$STATE)
math_scores_refined$STATE <- gsub('_', ' ', math_scores_refined$STATE)

state_shape_grade_4 <- map_data("state") %>% 
  rename(STATE = region) %>% 
  left_join(math_scores_refined, by = "STATE")

# this creates a heatmap of the Average 4th Grade Score per 4th grader for each 
# state from the years 2012 to 2015
ggplot(state_shape_grade_4) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = grade_4),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "Yellow", high = "Red") +
  labs(fill = "Average 4th Grade Scores") +
  blank_theme

state_shape_grade_8 <- map_data("state") %>% 
  rename(STATE = region) %>% 
  left_join(math_scores_refined, by = "STATE")

# this creates a heatmap of the Average 8th Grade Score per 8th grader for each 
# state from the years 2012 to 2015
ggplot(state_shape_grade_8) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = grade_8),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "Yellow", high = "Red") +
  labs(fill = "Average 8th Grade Scores") +
  blank_theme

the_max <- after_2000_refined %>%
  filter(exp_per_student == max(exp_per_student)) %>%
  pull(exp_per_student)
