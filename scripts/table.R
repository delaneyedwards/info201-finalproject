library("tidyverse")
library("knitr")

library("lintr")
library("styler")
states_all_extended <- read.csv("data/states_all_extended.csv")
states_all <- read.csv("data/states_all.csv")

avg_scores <- states_all %>%
  filter(YEAR == 2015) %>%
  select(STATE, 
         AVG_READING_4_SCORE,
         AVG_READING_8_SCORE)

scores_by_race <- states_all_extended %>%
  filter(YEAR == 2015) %>%
  group_by(STATE) %>%
  select(STATE, 
         INSTRUCTION_EXPENDITURE, 
         G04_WH_A_READING,
         G04_BL_A_READING,
         G04_HI_A_READING,
         G04_AS_A_READING,
         G04_AM_A_READING,
         G04_HP_A_READING,
         G04_TR_A_READING,
         G08_WH_A_READING,
         G08_BL_A_READING,
         G08_HI_A_READING,
         G08_AS_A_READING,
         G08_AM_A_READING,
         G08_HP_A_READING,
         G08_TR_A_READING) %>%
  rename(WHITE_GR04_READING = G04_WH_A_READING) %>%
  rename(BLACK_GR04_READING = G04_BL_A_READING) %>% 
  rename(LATINX_GR04_READING = G04_HI_A_READING) %>% 
  rename(ASIAN_GR04_READING = G04_AS_A_READING) %>% 
  rename(AMERICAN_INDIAN_ALASKA_NATIVE_GR04_READING = G04_AM_A_READING) %>% 
  rename(HAWAIIAN_NATIVE_PACIFIC_ISLANDER_GR04_READING = G04_HP_A_READING) %>% 
  rename(TWO_OR_MORE_RACES_GR04_READING = G04_TR_A_READING) %>%
  rename(WHITE_GR08_READING = G08_WH_A_READING) %>% 
  rename(BLACK_GR08_READING = G08_BL_A_READING) %>% 
  rename(LATINX_GR08_READING= G08_HI_A_READING) %>% 
  rename(ASIAN_GR08_READING = G08_AS_A_READING) %>% 
  rename(AMERICAN_INDIAN_ALASKA_NATIVE_GR08_READING = G08_AM_A_READING) %>% 
  rename(HAWAIIAN_NATIVE_PACIFIC_ISLANDER_GR08_READING = G08_HP_A_READING) %>% 
  rename(TWO_OR_MORE_RACES_GR08_READING = G08_TR_A_READING) 

states_table <- left_join(scores_by_race, avg_scores, by = "STATE") 
  
states_table <- states_table[-52, ]

states_table <- mutate(
  states_table, 
  WHITE = mean((WHITE_GR04_READING - AVG_READING_4_SCORE) +
                                (WHITE_GR08_READING - AVG_READING_8_SCORE)),
  BLACK = mean((BLACK_GR04_READING - AVG_READING_4_SCORE) +
                                    (BLACK_GR08_READING - AVG_READING_8_SCORE)),
  LATINX = mean((LATINX_GR04_READING - AVG_READING_4_SCORE) -
                                (LATINX_GR08_READING - AVG_READING_8_SCORE)),
  ASIAN = mean((ASIAN_GR04_READING - AVG_READING_4_SCORE) + 
                               (ASIAN_GR08_READING - AVG_READING_8_SCORE)),
  AMERICAN_INDIAN_ALASKA_NATIVE = mean(
    (AMERICAN_INDIAN_ALASKA_NATIVE_GR04_READING - AVG_READING_4_SCORE) + 
     (AMERICAN_INDIAN_ALASKA_NATIVE_GR08_READING - AVG_READING_8_SCORE)),
  HAWAIIAN_NATIVE_PACIFIC_ISLANDER = mean(
    (HAWAIIAN_NATIVE_PACIFIC_ISLANDER_GR04_READING - AVG_READING_4_SCORE) +
    (HAWAIIAN_NATIVE_PACIFIC_ISLANDER_GR08_READING - AVG_READING_8_SCORE)),
  TWO_OR_MORE_RACES = mean((TWO_OR_MORE_RACES_GR04_READING - 
    AVG_READING_4_SCORE) + (TWO_OR_MORE_RACES_GR08_READING - 
      AVG_READING_8_SCORE))
) %>%
  select(STATE,
         INSTRUCTION_EXPENDITURE,
         WHITE,
         BLACK,
         LATINX,
         ASIAN,
         AMERICAN_INDIAN_ALASKA_NATIVE,
         HAWAIIAN_NATIVE_PACIFIC_ISLANDER,
         TWO_OR_MORE_RACES
         ) 

table_states <- kable(states_table) 
 
 

                                      
