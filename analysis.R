# LOADING PACKAGES -------------------------------------------------------------
library(tidyverse)


# LOADING DATA -----------------------------------------------------------------
incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# VARIABLES --------------------------------------------------------------------
# The year that had highest population in jail
highest_jail_pop_year <- incarceration_df %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  pull(year)

# The year that had highest population in prison 
highest_prison_pop_year <- incarceration_df %>% 
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>% 
  pull(year)

# Highest BLACK population in prison 
highest_black_population_prison <- incarceration_df %>%
  summarise(max_black_pop_prison = max(black_prison_pop, na.rm = T)) %>%
  pull(max_black_pop_prison)

# Highest WHITE population in prison
highest_white_population_prison <- incarceration_df %>%
  summarise(max_white_pop_prison = max(white_prison_pop, na.rm = T)) %>%
  pull(max_white_pop_prison)

# Highest BLACK population in jail
highest_black_population_jail <- incarceration_df %>%
  summarise(max_black_pop_jail = max(black_jail_pop, na.rm = T)) %>%
  pull(max_black_pop_jail)

# Highest WHITE population in jail
highest_white_population_jail <- incarceration_df %>%
  summarise(max_white_pop_jail = max(white_jail_pop, na.rm = T)) %>%
  pull(max_white_pop_jail)

# State that has the highest BLACK population in jail
state_highest_black_jail <- incarceration_df %>%
  filter(black_jail_pop == max(na.omit(black_jail_pop))) %>%
  pull(state)

# State that has the highest WHITE population in jail
state_highest_white_jail <- incarceration_df %>%
  filter(white_jail_pop == max(na.omit(white_jail_pop))) %>%
  pull(state)

# State that has the highest BLACK population in prison
state_highest_black_prison <- incarceration_df %>%
  filter(black_prison_pop == max(na.omit(black_prison_pop))) %>%
  pull(state)

# State that has the highest WHITE population in prison
state_highest_white_prison <- incarceration_df %>%
  filter(white_prison_pop == max(na.omit(white_prison_pop))) %>%
  pull(state)


# TRENDS OVER TIME CHART -------------------------------------------------------
# Selecting variables from the incarceration_df and making smaller dataframe
trend_chart_df <- data.frame(
  incarceration_df %>% 
    select(year, state, black_prison_pop)
)

# Found that "NY" and "CA" has the highest population in prison from the codes above
trend_filter_states <- trend_chart_df %>% 
  filter(state %in% c("NY","CA"))

# Building a chart that displays trend of BLACK prison population of "NY" and "CA"
trend_chart <- ggplot(filter_states, aes(x=year, y=black_prison_pop)) + 
  geom_point(aes(col = state)) +
  labs(title = "Trend of Black Prisoner Population in NY and CA", x = "Year", y = "Black Prisoner Population", color = "State") 


# VARIABLE COMPARISON CHART ----------------------------------------------------
# Selecting variables from the incarceration_df and making smaller dataframe
comparison_chart_df <- data.frame(
  incarceration_df %>% 
    select(year, state, black_prison_pop, black_pop_15to64)
)

comparison_filter_states <- comparison_chart_df %>%
  filter(state == "CA")

# Building a chart that displays the relationship between black population and black prisoner in CA
variable_comparison_chart <-ggplot(comparison_filter_states, aes(x=black_pop_15to64, y=black_prison_pop))+ 
  geom_point()+
  geom_smooth(method = "loess", formula = y ~ x, se=FALSE, fullrange=TRUE)+
  labs(title="title",  x="Population 15 to 64", y = "Black Prisoner")


# MAP --------------------------------------------------------------------------


