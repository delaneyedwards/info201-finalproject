setwd("C:\\Users\\delan\\Documents\\INFO201\\project-kellenmcgibbon\\data")
states_data <- read.csv("states_all.csv")
states_extended_data <- read.csv("states_all_extended.csv")
install.packages("tidyr")
library("tidyr")
library("dplyr")
install.packages("ggplot2") 
library("ggplot2")   

recent <- states_extended_data %>% 
  filter(YEAR > 2000) %>% 
  select(STATE, YEAR, G08_A_A_MATHEMATICS, G08_WH_A_MATHEMATICS, G08_BL_A_MATHEMATICS,
         G08_HI_A_MATHEMATICS, G08_AS_A_MATHEMATICS, G08_AM_A_MATHEMATICS, 
         G08_HP_A_MATHEMATICS, G08_TR_A_MATHEMATICS)

averaging_total <- recent %>% 
  group_by(STATE) %>% 
  summarize(Average_Math_Score = mean(G08_A_A_MATHEMATICS, na.rm = T))

average_white <-recent %>% 
  group_by(STATE) %>% 
  summarize(Average_White_Math_Score = mean(G08_WH_A_MATHEMATICS, na.rm = T))

average_black <- recent %>% 
  group_by(STATE) %>% 
  summarize(Average_Black_Math_Score = mean(G08_BL_A_MATHEMATICS, na.rm = T))

average_latinx <- recent %>% 
  group_by(STATE) %>% 
  summarize(Average_Latinx_Math_Score = mean(G08_HI_A_MATHEMATICS, na.rm = T))

average_asian <- recent %>% 
  group_by(STATE) %>% 
  summarize(Average_Asian_Math_Score = mean(G08_AS_A_MATHEMATICS, na.rm = T))

average_native <- recent %>% 
  group_by(STATE) %>% 
  summarize(Average_Native_Math_Score = mean(G08_AM_A_MATHEMATICS, na.rm = T))

average_hawaiian <- recent %>% 
  group_by(STATE) %>% 
  summarize(Average_Hawaiian_Math_Score = mean(G08_HP_A_MATHEMATICS, na.rm = T))

average_mixed <- recent %>% 
  group_by(STATE) %>% 
  summarize(Average_Mixed_Math_Score = mean(G08_TR_A_MATHEMATICS, na.rm = T))

all_averages <- averaging_total %>% 
  left_join(average_asian) %>% 
  left_join(average_white) %>% 
  left_join(average_black) %>% 
  left_join(average_latinx) %>% 
  left_join(average_native) %>% 
  left_join(average_hawaiian) %>% 
  left_join(average_mixed)

ggplot(data = all_averages) +
  geom_col(mapping = aes(x = STATE, y = Average_Math_Score))

  
  
