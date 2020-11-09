library("tidyr")
library("dplyr")
library("ggplot2")

enrollment <- read.csv("data/Enrollment.csv")
GT <- read.csv("data/Gifted\ and\ Talented.csv")
AP <- read_csv("data/Advanced\ Placement.csv")
IB <- read_csv("data/International\ Baccalaureate.csv")

enrollment_demographics <- enrollment %>% 
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
  mutate("Hispanic or Latino of any race" = SCH_ENR_HI_M + SCH_ENR_HI_F, .keep = "unused") %>% 
  mutate("American Indian or Alaska Native" = SCH_ENR_AM_M + SCH_ENR_AM_F, .keep = "unused") %>% 
  mutate("Asian" = SCH_ENR_AS_M + SCH_ENR_AS_F, .keep = "unused") %>% 
  mutate("Native Hawaiian or Other Pacific Islander" = SCH_ENR_HP_M + SCH_ENR_HP_F, .keep = "unused") %>% 
  mutate("Black or African American" = SCH_ENR_BL_M + SCH_ENR_BL_F, .keep = "unused") %>% 
  mutate("White" = SCH_ENR_WH_M + SCH_ENR_WH_F, .keep = "unused") %>% 
  mutate("Two or more races" = SCH_ENR_TR_M + SCH_ENR_TR_F, .keep = "unused") %>% 
  gather(
    key = Demographic,
    value = count, 
  ) %>% 
  group_by(Demographic) %>% 
  summarise(total = sum(count, na.rm = TRUE))
  
enrollment_pie <- ggplot(enrollment_demographics, aes(x = "", y = total, fill = Demographic)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + 
  labs(title = "Student Enrollment", subtitle = "Proportion of students enrolled in all schools and justice facilities, preschool-grade 12") +
  geom_text(aes(label = paste(round(total / sum(total) * 100, 1), "%"), x = 1.4), position = position_stack(vjust = 0.5), size = 3)

GT_demographics <- GT %>% 
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
  mutate("Hispanic or Latino of any race" = SCH_GTENR_HI_M + SCH_GTENR_HI_F, .keep = "unused") %>% 
  mutate("American Indian or Alaska Native" = SCH_GTENR_AM_M + SCH_GTENR_AM_F, .keep = "unused") %>% 
  mutate("Asian" = SCH_GTENR_AS_M + SCH_GTENR_AS_F, .keep = "unused") %>% 
  mutate("Native Hawaiian or Other Pacific Islander" = SCH_GTENR_HP_M + SCH_GTENR_HP_F, .keep = "unused") %>% 
  mutate("Black or African American" = SCH_GTENR_BL_M + SCH_GTENR_BL_F, .keep = "unused") %>% 
  mutate("White" = SCH_GTENR_WH_M + SCH_GTENR_WH_F, .keep = "unused") %>% 
  mutate("Two or more races" = SCH_GTENR_TR_M + SCH_GTENR_TR_F, .keep = "unused") %>% 
  gather(
    key = Demographic,
    value = count, 
  ) %>% 
  group_by(Demographic) %>% 
  summarise(total = sum(count, na.rm = TRUE))

GT_pie <- ggplot(GT_demographics, aes(x = "", y = total, fill = Demographic)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + 
  labs(title = "Gifted and Talented Enrollment", subtitle = "Proportion of students enrolled in gifted and talented programs, preschool-grade 12") +
  geom_text(aes(label = paste(round(total / sum(total) * 100, 1), "%"), x = 1.4), position = position_stack(vjust = 0.5), size = 3)


AP_demographics <- AP %>% 
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
  mutate("Hispanic or Latino of any race" = SCH_APENR_HI_M + SCH_APENR_HI_F, .keep = "unused") %>% 
  mutate("American Indian or Alaska Native" = SCH_APENR_AM_M + SCH_APENR_AM_F, .keep = "unused") %>% 
  mutate("Asian" = SCH_APENR_AS_M + SCH_APENR_AS_F, .keep = "unused") %>% 
  mutate("Native Hawaiian or Other Pacific Islander" = SCH_APENR_HP_M + SCH_APENR_HP_F, .keep = "unused") %>% 
  mutate("Black or African American" = SCH_APENR_BL_M + SCH_APENR_BL_F, .keep = "unused") %>% 
  mutate("White" = SCH_APENR_WH_M + SCH_APENR_WH_F, .keep = "unused") %>% 
  mutate("Two or more races" = SCH_APENR_TR_M + SCH_APENR_TR_F, .keep = "unused") %>% 
  gather(
    key = Demographic,
    value = count, 
  ) %>% 
  group_by(Demographic) %>% 
  summarise(total = sum(count, na.rm = TRUE))

AP_pie <- ggplot(AP_demographics, aes(x = "", y = total, fill = Demographic)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + 
  labs(title = "AP Enrollment", subtitle = "Proportion of students enrolled in an AP program, grade 9-12") +
  geom_text(aes(label = paste(round(total / sum(total) * 100, 1), "%"), x = 1.4), position = position_stack(vjust = 0.5), size = 3)

IB_demographics <- IB %>% 
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
  mutate("Hispanic or Latino of any race" = SCH_IBENR_HI_M + SCH_IBENR_HI_F, .keep = "unused") %>% 
  mutate("American Indian or Alaska Native" = SCH_IBENR_AM_M + SCH_IBENR_AM_F, .keep = "unused") %>% 
  mutate("Asian" = SCH_IBENR_AS_M + SCH_IBENR_AS_F, .keep = "unused") %>% 
  mutate("Native Hawaiian or Other Pacific Islander" = SCH_IBENR_HP_M + SCH_IBENR_HP_F, .keep = "unused") %>% 
  mutate("Black or African American" = SCH_IBENR_BL_M + SCH_IBENR_BL_F, .keep = "unused") %>% 
  mutate("White" = SCH_IBENR_WH_M + SCH_IBENR_WH_F, .keep = "unused") %>% 
  mutate("Two or more races" = SCH_IBENR_TR_M + SCH_IBENR_TR_F, .keep = "unused") %>% 
  gather(
    key = Demographic,
    value = count, 
  ) %>% 
  group_by(Demographic) %>% 
  summarise(total = sum(count, na.rm = TRUE))

IB_pie <- ggplot(IB_demographics, aes(x = "", y = total, fill = Demographic)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + 
  labs(title = "International Baccalaureate (IB) Diploma Programme Enrollment", subtitle = "Proportion of students enrolled in an IB Diploma Programme, grade 9-12") +
  geom_text(aes(label = paste(round(total / sum(total) * 100, 1), "%"), x = 1.4), position = position_stack(vjust = 0.5), size = 3)


  

