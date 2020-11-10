
states_data <- read.csv("data/states_all.csv")
states_extended_data <- read.csv("data/states_all_extended.csv")

library("tidyr")
library("dplyr")
library("ggplot2")   


math_race <- states_extended_data %>%
  filter(YEAR > 2011 & YEAR < 2016) %>% 
  select(G08_WH_A_MATHEMATICS, 
         G08_BL_A_MATHEMATICS,
         G08_HI_A_MATHEMATICS, 
         G08_AS_A_MATHEMATICS, 
         G08_AM_A_MATHEMATICS, 
         G08_HP_A_MATHEMATICS, 
         G08_TR_A_MATHEMATICS) %>% 
  rename(White = G08_WH_A_MATHEMATICS) %>% #organizing scores by race
  rename("Black/African American" = G08_BL_A_MATHEMATICS) %>% 
  rename(Latinx = G08_HI_A_MATHEMATICS) %>% 
  rename(Asian = G08_AS_A_MATHEMATICS) %>% 
  rename("American Indian/Alaska Native" = G08_AM_A_MATHEMATICS) %>% 
  rename("Hawaiian Native/Pacific Islander" = G08_HP_A_MATHEMATICS) %>% 
  rename("Two or More Races" = G08_TR_A_MATHEMATICS) %>% 
  gather(key = Race, value = Score) %>% #shift table to be long
  group_by(Race) %>% 
  summarize(Score = mean(Score, na.rm = T)) #average the scores over time
  

ggplot(data = math_race) +
  geom_col(mapping = aes(x = Race, y = Score)) +
  labs(title = "Average NAEP Math Score by Race")

reading_race <- states_extended_data %>%
  filter(YEAR > 2011 & YEAR < 2016) %>% 
  select(G08_WH_A_READING, 
         G08_BL_A_READING,
         G08_HI_A_READING, 
         G08_AS_A_READING, 
         G08_AM_A_READING, 
         G08_HP_A_READING, 
         G08_TR_A_READING) %>% 
  rename(White = G08_WH_A_READING) %>% #renaming by race to be readable
  rename("Black/African American" = G08_BL_A_READING) %>% 
  rename(Latinx = G08_HI_A_READING) %>% 
  rename(Asian = G08_AS_A_READING) %>% 
  rename("American Indian/Alaska Native" = G08_AM_A_READING) %>% 
  rename("Hawaiian Native/Pacific Islander" = G08_HP_A_READING) %>% 
  rename("Two or More Races" = G08_TR_A_READING) %>% 
  gather(key = Race, value = Score) %>% #shift table to be long (i.e. race as row)
  group_by(Race) %>% 
  summarize(Score = mean(Score, na.rm = T)) #average the scores over time


ggplot(data = reading_race) +
  geom_col(mapping = aes(x = Race, y = Score)) +
  labs(title = "Average NAEP Reading Score by Race")

  
  
