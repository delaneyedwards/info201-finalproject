library("tidyverse")
library("dplyr")

data_states_extended <- read.csv("data/states_all_extended.csv")
data_states <- read.csv("data/states_all.csv")
data_2015 <- data_states %>%
  filter(YEAR == 2015)
data_2015_extended <- data_2015 %>%
  mutate(exp_per_student = INSTRUCTION_EXPENDITURE / ENROLL) %>%
  mutate(avg_test_score_8 = (AVG_MATH_8_SCORE + AVG_READING_8_SCORE) / 2)

# list of the values in the summary information
summary_info <- list()

# Value 1
# the state with the highest expenditure per student and its value
state_highest_per_student_expend <- data_2015_extended %>%
  filter(exp_per_student == max(exp_per_student, na.rm = T)) %>%
  select(STATE)
state_highest_per_student_expend
highest_per_student_expend <- data_2015_extended %>%
  filter(exp_per_student == max(exp_per_student, na.rm = T)) %>%
  pull(exp_per_student)
highest_per_student_expend
summary_info$highest_exp_per_student <- highest_per_student_expend

# value 2
# the state with the lowest expenditure per student and its value
state_lowest_per_student_expend <- data_2015_extended %>%
  filter(exp_per_student == min(exp_per_student, na.rm = T)) %>%
  select(STATE)
state_lowest_per_student_expend
lowest_per_student_expend <- data_2015_extended %>%
  filter(exp_per_student == min(exp_per_student, na.rm = T)) %>%
  pull(exp_per_student)
lowest_per_student_expend
summary_info$lowest_exp_per_student <- lowest_per_student_expend

# creates dataset math_scores_refined
scores_refined_2015 <- data_states %>%
  filter(YEAR == 2015) %>%
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
  )
scores_refined_2015$STATE <- tolower(scores_refined_2015$STATE)
scores_refined_2015$STATE <- gsub("_", " ", scores_refined_2015$STATE)

# value 3
# difference in ang 8th grade test scores between the state with highest
# total expenditure
# vs lowest total expenditure
ny_avg_scores_8 <- scores_refined_2015 %>%
  filter(STATE == "new york") %>%
  pull(grade_8)
ny_avg_scores_8
arizona_avg_scores_8 <- scores_refined_2015 %>%
  filter(STATE == "arizona") %>%
  pull(grade_8)
arizona_avg_scores_8
summary_info$diff_high_low_expend_scores <- abs(
  ny_avg_scores_8 - arizona_avg_scores_8
)

# value 4
# state with the highest 8th grade test scores
state_high_math <- data_2015 %>%
  filter(AVG_MATH_8_SCORE == max(AVG_MATH_8_SCORE)) %>%
  select(STATE)
state_high_math

# value 5
# the differences in the test scores between highest scoring state and
# lowest scoring state
highest_test_scores <- scores_refined_2015 %>%
  filter(STATE == "massachusetts") %>%
  pull(grade_8)
highest_test_scores
summary_info$state_highest_score <- highest_test_scores
lowest_test_scores <- scores_refined_2015 %>%
  filter(STATE == "district of columbia") %>%
  pull(grade_8)
lowest_test_scores
diff_math_scores <- abs(highest_test_scores - lowest_test_scores)
diff_math_scores
summary_info$range_test_scores <- abs(highest_test_scores - lowest_test_scores)
