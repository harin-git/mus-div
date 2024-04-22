#' This script provides supplementary analyses on accounting for different biases that may arise from the data
#' Tested on French users. Raw streams user information are not available as open access.

# load project wide packages and functions
source('utils.R')

################################################################################
# 1. Does result change with more strict or less strict user filtering?
################################################################################
fr_users <- readRDS('data/private/user/fr_user.rds') %>% rename(regional_group = NUTS3)
fr_streams <- readRDS('data/private/stream/fr_stream_1month_LAN.rds')

# plot the general distribution of number geo locations and percentage play
n_geo_hist <- fr_users %>%
  filter(n_geo_loc < 50) %>%
  ggplot(aes(n_geo_loc)) +
  geom_histogram(bins = 50) +
  labs(x = 'Number of unique geolocations', y = 'Frequency')

geo_threshold_hist <- fr_users %>%
  ggplot(aes(loc_play_percent)) +
  geom_histogram(bins = 50) +
  labs(x = 'Streams proportion in top geolocation (%)', y = 'Frequency')

(joint_hist <- n_geo_hist + geo_threshold_hist)

# save plot for SI figure
joint_hist %>% plot_save('SI/geolocation_histogram', c(183, 50))


# define different levels of filtering users
# in paper, we filter only users with less than 10 geo locations
n_geo <- c(1, 2, 5, 10, Inf)

storage = list()
for (i in 1:length(n_geo)) {
  wid <- fr_users %>%
    filter(inv_gs_score < 75) %>%
    filter(n_geo_loc <= n_geo[i]) %>%
    group_by(regional_group) %>%
    summarise(WID = mean(inv_gs_score, na.rm = T),
              pop_size = n_distinct(study_user_id))
  
  bid <- fr_streams %>%
    left_join(fr_users %>% distinct(study_user_id, regional_group)) %>%
    group_by(regional_group) %>%
    sample_n(10000, replace = TRUE) %>% # balance the number of streams per city
    summarise(BID = hill(table(product_id), q = 1))
  
  storage[[i]] <- left_join(wid, bid) %>% cbind(n_geo = n_geo[i])
  
  print(i)
}
geo_output <- do.call(rbind, storage)

geo_cor <- geo_output %>%
  pivot_longer(c(WID, BID)) %>%
  group_by(n_geo, name) %>%
  summarise(cor = cor(pop_size, value))

(geo_cor_p <- geo_cor %>%
  mutate(n_geo = as.factor(n_geo)) %>%
  ggplot(aes(n_geo, cor)) +
  geom_col() +
  facet_wrap(~ name) +
  labs(x = 'Geolocation filtering threshold', y = 'Pearson correlation'))

# save plot for SI figure
geo_cor_p %>% plot_save('SI/geo_location_threshold_correlation', c(100, 50))


################################################################################
# 2. Does result change when excluding algorithmic streams?
################################################################################
algo_stream <- fr_streams %>%
  left_join(fr_users %>% distinct(study_user_id, regional_group)) %>%
  group_by(regional_group) %>%
  mutate(pop_size = n_distinct(study_user_id)) %>%
  group_by(regional_group, pop_size, listen_type) %>%
  sample_n(10000, replace = TRUE) %>% # balance the number of streams per city
  summarise(BID = hill(table(product_id), q = 1))

(algo_p <- algo_stream %>%
  na.omit() %>%
  mutate(listen_type = factor(listen_type, levels = c('O', 'A'), labels = c('Organic', 'Algorithmic'))) %>%
  ggplot(aes(log10(pop_size), BID)) +
  geom_point(size = 0.5) +
  ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001, cor.coef.name = 'r') +
  geom_smooth(method = 'glm', size = 0.7) +
  labs(x = 'Population size (log base 10)') +
  scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
  annotation_logticks(
    sides = 'b'
  ) +
  facet_wrap(~ listen_type))

# save plot for SI figure
algo_p %>% plot_save('SI/algorithmic_organic_streams', c(183, 80))



################################################################################
# 3. Does result change when using census population and density instead of population size derived from n users?
################################################################################
fr_boot <- read.csv('data/diversity/FR_diversity.csv')
fr_census <- read.csv('data/census/fr/fr_nuts3_meta.csv')

pop_join <- fr_boot %>% filter(metric %in% c('ind_div', 'song_div_q1')) %>%
  left_join(fr_census, by = c('regional_group' = 'NUTS3'))
pop_join <- pop_join %>% mutate(metric = factor(metric, labels = c('WID', 'BID')))

(census_pop <- pop_join %>%
  ggplot(aes(log10(population), boot_m)) +
  geom_point() +
  ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001, cor.coef.name = 'r') +
  geom_smooth(method = 'glm', size = 0.7) +
  labs(x = 'Census population size (log base 10)', y = 'Diversity') +
  scale_x_continuous(labels = math_format(10 ^ .x)) +
    annotation_logticks(
      sides = 'b'
    ) +
  facet_wrap(~ metric, scale = 'free'))

(census_pop_density <- pop_join %>%
    ggplot(aes(log10(population_density), boot_m)) +
    geom_point() +
    ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001, cor.coef.name = 'r') +
    geom_smooth(method = 'glm', size = 0.7) +
    labs(x = 'Census population density (log base 10)', y = 'Diversity') +
    scale_x_continuous(labels = math_format(10 ^ .x)) +
    annotation_logticks(
      sides = 'b'
    ) +
    facet_wrap(~ metric, scale = 'free'))

census_join <- census_pop / census_pop_density

# save plot for SI figure
census_join %>% plot_save('SI/census_population_correlation', c(183, 120))
