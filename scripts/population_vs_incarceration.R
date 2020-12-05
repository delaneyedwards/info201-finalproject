# What is the relationship between the size of a state and incarceration rates in 2018?

library("ggplot2")
library("dplyr")

incarceration_trends <- read_csv("incarceration_trends.csv")

incarceration_by_state <- incarceration_trends %>% 
  filter(year == "2018") %>% 
  group_by(state, region) %>% 
  summarise(
    avg_incarceration_rate = mean(total_jail_pop_rate, na.rm = TRUE),
    total_pop = sum(total_pop)
  )


ggplot(incarceration_by_state) +
  geom_text(mapping = aes(x = total_pop, y = avg_incarceration_rate, label = state, color = region)) + 
  theme_minimal() +
  labs(
    x = "Population", y = "Jail Incarceration Rate (Per 100,000 Residents Age 15-64)",
    color = "Region", title = "State Population vs. Jail Incarceration Rate"
  )
  