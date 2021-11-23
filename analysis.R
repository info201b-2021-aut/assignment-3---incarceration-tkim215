# LOADING PACKAGES -------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(maps)


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
trend_chart <- ggplot(trend_filter_states, aes(x=year, y=black_prison_pop)) + 
  geom_point(aes(col = state)) +
  labs(title = "Trend of Black Prisoner Population in NY and CA", x = "Year", y = "Black Prisoner Population", color = "State") 


# VARIABLE COMPARISON CHART ----------------------------------------------------
# Selecting variables from the incarceration_df and making smaller dataframe
comparison_chart_df <- data.frame(
  incarceration_df %>% 
    select(year, state, black_prison_pop, black_pop_15to64)
)

# Filtering the state, California (CA)
comparison_filter_states <- comparison_chart_df %>%
  filter(state == "CA")

# Building a chart that displays the relationship between black population and black prisoner in CA
variable_comparison_chart <-ggplot(comparison_filter_states, aes(x=black_pop_15to64, y=black_prison_pop)) + 
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(title="Relationship between Black Population and Black Prisoner in CA",
       x="Black Population between 15 and 64", y = "Black Prisoner")


# MAP --------------------------------------------------------------------------
# Selecting variables from incarceration_df to make smaller dataframe
map_df <- incarceration_df %>%
  filter(year == 2018) %>%
  select(year, fips, black_jail_pop)

# USing county.fips function for new dataframe
county_df <- county.fips %>%
  separate(polyname, c("polyname", "sub"))

# Importing map of United States divided in counties
state_df <- map_data("county") %>%
  rename(polyname = region) %>%
  full_join(county_df, by = "polyname")

merged_df <- full_join(state_df, map_df, by = "fips")

# Building a chart(map) displays the population of black jail population in 2018
map_chart <- ggplot(merged_df) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop), color = "grey", size = 0.3) +
  coord_map()+
  scale_colour_gradient2(low = "white", mid = "yellow", high = "red", guide = "colorbar", aesthetics = "fill") + 
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  ggtitle("Black Jail Population in 2018")
