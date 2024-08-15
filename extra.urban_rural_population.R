#' This script tests how the urban versus rural population proportion in France, Brazil and Germany
#' compares to other countries around the world. Related to discussion of urban vs. rural dynamics and
#' the representativeness of our the countries in our study.

# load project wide packages and functions
source('utils.R')

# load data
urban_rural <- read.csv('data/census/urban_rural_population.csv')

# take the latest year
urban_rural <- urban_rural[urban_rural$Year == max(urban_rural$Year),]

# compute urban to rural ratio and quantile
urban_rural <- urban_rural %>%
  mutate(urban_prop = Urban.population / (Urban.population + Rural.population) * 100)

# what is the quantile?
(quantile(urban_rural$urban_prop, na.rm = TRUE, probs = seq(0, 1, 0.1)))

# what is the global average?
(global_urban_m <- mean(urban_rural$urban_prop, na.rm = TRUE))
(global_urban_sd <- sd(urban_rural$urban_prop, na.rm = TRUE))

# what about the three countries
(urban_rural <- urban_rural %>%
  filter(Entity %in% c('France', 'Brazil', 'Germany')))

