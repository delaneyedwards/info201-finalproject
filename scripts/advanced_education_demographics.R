library("tidyr")
library("dplyr")
library("ggplot2")

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

# Combines all data frames into a single data frame.
all_race <- rbind(enrollment_race, gt_race, ap_race, ib_race)

# Creates a stacked bar chart of all enrollment data.
graph <- ggplot(all_race) +
  geom_col(
    mapping = aes(x = program, y = total, fill = race), position = "fill"
  ) +
  labs(
    x = "Program", y = "Proportion", fill = "Race",
    title = "Distribution of Race Across Educational Programs"
  )
