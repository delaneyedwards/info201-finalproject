library("tidyverse")
library("dplyr")
project_states <- read.csv("data/states_all_extended.csv")
data_states <- read.csv("data/states_all.csv")




states_refined <- data_states %>%
  filter(YEAR > 2011 & YEAR < 2016) %>%

#list of the values in the summary information
summary_info <- list()

after_2000_refined

#highest per student expenditure vs the lowest per student expenditure

state_max_expend <- after_2000_refined %>%
  filter(exp_per_student == max(exp_per_student)) %>%
  pull(STATE)
state_max_expend



state_min_expend <- after_2000_refined %>%
  filter(exp_per_student == min(exp_per_student)) %>%
  pull(STATE)
state_min_expend

value_max_expend <- after_2000_refined %>%
  filter(exp_per_student == max(exp_per_student)) %>%
  pull(exp_per_student)
value_max_expend

value_min_expend <- after_2000_refined %>%
  filter(exp_per_student == min(exp_per_student)) %>%
  pull(exp_per_student)
state_min_expend



summary_info$range_per_student_exp <- value_max_expend - value_min_expend

#the differnces in the test scores between these states

state_high_test_scores <- math_scores_refined %>%
  filter(STATE == "new york") %>%
  pull(mean_math_8)
state_high_test_scores

state_low_test_scores <- math_scores_refined %>%
  filter(STATE == "idaho") %>%
  pull(mean_math_8)
state_high_test_scores

diff_math_scores <- abs(state_high_test_scores - state_low_test_scores)


#state with the highest 8th grade test scores



#the perentage of race in that state

#value 5



