library("tidyverse")
library("ggplot2")
install.packages("mapproj")

setwd("C:\\Users\\sahit\\Desktop\\info201-groupproject\\data")
data_states <- read.csv("states_all.csv")
data_states_extended <- read.csv("states_all_extended.csv")

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

install.packages("maps")

state_shape <- map_data("state") %>% 
  rename(STATE = region) %>% 
  left_join(after_2000_refined, by = "STATE")

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

ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = exp_per_student),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "Yellow", high = "Red") +
  labs(fill = "Average amount of money per Child") +
  blank_theme

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

math_scores_refined$STATE <- tolower(math_scores_refined$STATE)
math_scores_refined$STATE <- gsub('_', ' ', math_scores_refined$STATE)

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
