library(tidyverse)

# Loading Data
incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Number of columns and rows 
obs_incarceration <- nrow(incarceration_df)
num_features_incarceration <- ncol(incarceration_df)

