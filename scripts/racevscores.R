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
  select(G08_WH_A_MATHEMATICS, G08_BL_A_MATHEMATICS,
         G08_HI_A_MATHEMATICS, G08_AS_A_MATHEMATICS, G08_AM_A_MATHEMATICS, 
         G08_HP_A_MATHEMATICS, G08_TR_A_MATHEMATICS) %>% 
  rename(White = G08_WH_A_MATHEMATICS) %>% 
  rename(Black = G08_BL_A_MATHEMATICS) %>% 
  rename(Latinx = G08_HI_A_MATHEMATICS) %>% 
  rename(Asian = G08_AS_A_MATHEMATICS) %>% 
  rename(Native = G08_AM_A_MATHEMATICS) %>% 
  rename(Hawaiian = G08_HP_A_MATHEMATICS) %>% 
  rename(Mixed = G08_TR_A_MATHEMATICS) %>% 
  gather(key = Race, value = Score) %>% 
  group_by(Race) %>% 
  summarize(Score = mean(Score, na.rm = T))
  

ggplot(data = recent) +
  geom_col(mapping = aes(x = Race, y = Score)) %>% 
  labs(title = "Average NAEP Math Score by Race Since 2000")

  
  
