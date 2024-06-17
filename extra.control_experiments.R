#' This script provides supplementary analyses on accounting for different biases that may arise from the data
#' Tested on French users. Raw streams user information are not available as open access.

# load project wide packages and functions
source('utils.R')

################################################################################
# VARYING USER FILTERING CRITERIA
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
# ALGORITHMIC BIASES
################################################################################
algo_bias <- function(country){
  col_pal <- switch(
    country,
    'fr' = c('#99a8ba', '#4c6787', FR_COL1),
    'br' = c('#99d5af', '#4cb674', BR_COL1),
    'de' = c('#ffcc00', '#b28e00', '#665100')
  )
  
  
  ## DATA SET UP
  # load user data
  users <- readRDS(sprintf('data/private/user/%s_user.rds', country))
  if(country == 'br'){
    users <- rename(users, regional_group = muni_name)
  } else {
    users <- rename(users, regional_group = NUTS3)
  }

  # load stream data  
  streams <- readRDS(sprintf('data/private/stream/%s_stream_1month_LAN.rds', country))
  
  # filter only regions where more than 200 people can be used for aggregation.
  # this is the same criteria as done with general correlations
  min_user_per_region <- 200
  all_regions <- users$regional_group %>% unique()
  
  # very high values of GS score is noise. Ambiguous GS-scores are coded 100
  max_geo_loc <- 10
  select_region <- users %>%
    filter(!is.na(inv_gs_score), inv_gs_score < 75, n_geo_loc < max_geo_loc) %>%
    group_by(regional_group) %>%
    filter(n() >= min_user_per_region)
  select_region <- select_region$regional_group %>% unique()
  
  message(sprintf('There are %s regional groups in %s', length(select_region), country))
  
  user_clean <- users %>%
    filter(regional_group %in% select_region) %>%
    select(study_user_id, regional_group, gender, age)
  
  # update streams
  streams <- streams %>% filter(study_user_id %in% user_clean$study_user_id)
  
  
  ## TESTING
  algo_dt <- streams %>%
    left_join(user_clean %>% distinct(study_user_id, regional_group, age, gender)) %>%
    # make age_category as 15~30, 30~45, 45~60
    mutate(age_category = case_when(
      age > 15 & age <= 30 ~ '15~30',
      age > 30 & age <= 45 ~ '30~45',
      age > 46 & age <= 60 ~ '45~60'
    )) %>%
    group_by(regional_group) %>%
    mutate(pop_size = n_distinct(study_user_id))
  
  # Algorithmic vs. organic streams
  message('computing BID...')
  algo_orga <- algo_dt %>%
    group_by(regional_group, pop_size, listen_type) %>%
    sample_n(10000, replace = TRUE) %>% # balance the number of streams per city
    summarise(BID = hill(table(product_id), q = 1))
  
  (algo_orga_p <- algo_orga %>%
      na.omit() %>%
      mutate(listen_type = factor(listen_type, levels = c('O', 'A'), labels = c('Organic', 'Algorithmic'))) %>%
      ggplot(aes(log10(pop_size), BID)) +
      geom_point(size = 0.5, colour = col_pal[3]) +
      ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001, cor.coef.name = 'r', colour = col_pal[3]) +
      geom_smooth(method = 'glm', size = 0.7, colour = col_pal[3]) +
      labs(x = 'Population size (log base 10)') +
      scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
      annotation_logticks(
        sides = 'b'
      ) +
      facet_wrap(~ listen_type))
  
  # Amount of algorithmic streams per region by age group
  message('computing age level difference...')
  algo_freq <- algo_dt %>%
    filter(!is.na(age_category)) %>%
    group_by(regional_group, pop_size, age_category) %>%
    sample_n(10000, replace = TRUE) %>%
    summarise(algo_freq = sum(listen_type == 'A') / n() * 100)
  
  (algo_freq_p <- algo_freq %>%
      ggplot(aes(log10(pop_size), algo_freq, colour = age_category, group = age_category)) +
      # geom_point(size = 0.5) +
      ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001, cor.coef.name = 'r') +
      geom_smooth(method = 'glm', size = 0.7) +
      labs(x = 'Population size (log base 10)', y = 'Proportion algorithmic (%)', colour = 'Age group') +
      scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
      annotation_logticks(
        sides = 'b'
      ) +
      scale_colour_manual(values = col_pal) +
      theme(legend.position = c(.8, .8)))
  
  # Amount of algorithmic streams per region by gender
  message('computing gender difference...')
  algo_gen <- algo_dt %>%
    filter(gender %in% c('M', 'F')) %>%
    group_by(regional_group, pop_size, gender) %>%
    sample_n(10000, replace = TRUE) %>%
    summarise(algo_freq = sum(listen_type == 'A') / n() * 100) %>%
    mutate(gender = ifelse(gender == 'M', 'Male', 'Female'))
  
  (algo_gen_p <- algo_gen %>%
      ggplot(aes(log10(pop_size), algo_freq, group = gender)) +
      ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001, cor.coef.name = 'r', colour = col_pal[3]) +
      geom_smooth(aes(linetype = gender), method = 'glm', size = 0.7, colour = col_pal[3]) +
      labs(x = 'Population size (log base 10)', y = 'Proportion algorithmic (%)', linetype = 'Gender') +
      scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
      annotation_logticks(
        sides = 'b'
      ) +
      theme(legend.position = c(.8, .8)))
  
  
  # # variations in use of recommendation per region across users
  # algo_var <- algo_dt %>%
  #   filter(!is.na(age_category)) %>%
  #   group_by(regional_group, pop_size, study_user_id, age_category) %>%
  #   summarise(algo_freq = sum(listen_type == 'A') / n() * 100) %>%
  #   group_by(regional_group, pop_size, age_category) %>%
  #   summarise(algo_var = sd(algo_freq))
  # 
  # (algo_var_p <- algo_var %>%
  #     ggplot(aes(log10(pop_size), algo_var, colour = age_category, group = age_category)) +
  #     # geom_point(size = 0.5) +
  #     ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001, cor.coef.name = 'r') +
  #     geom_smooth(method = 'glm', size = 0.7) +
  #     labs(x = 'Population size (log base 10)', y = 'Variations in use of algorithms (SD)', colour = 'Age category') +
  #     scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
  #     annotation_logticks(
  #       sides = 'b'
  #     ) +
  #     scale_colour_manual(values = col_pal) +
  #     theme(legend.position = 'right'))
  
  # gather all plots and return
  algo_orga_p + algo_freq_p + algo_gen_p + plot_layout(widths =  c(2, 1, 1))
}

fr_algo <- algo_bias('fr')
br_algo <- algo_bias('br')
de_algo <- algo_bias('de')

# join the country plots
joint_algo <- fr_algo / br_algo / de_algo

# save plot for SI figure
joint_algo %>% plot_save('SI/algorithmic_organic_streams', c(280, 200))



################################################################################
# POPULATION DENSITY AND CENSUS POPULATION
################################################################################
# Does result change when using census population and density instead of population size derived from n users?
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
