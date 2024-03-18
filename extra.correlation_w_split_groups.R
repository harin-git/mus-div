#' This script is supplmentary to the casual inference where each of the covariates are split into groups
#' and correlation between BID/WID and population size is tested

# load project wide packages and functions
source('utils.R')

# load DAG data
dag_data <- readRDS('data_output/dag_data.rds')

# make dataframe of covariates
covar <- dag_data %>% 
  ungroup() %>%
  # select with the right names
  select(
    NUTS3,
    BID = BID_nuts,
    WID,
    mus = log_n_music_venues,
    pops = log_pop_size_nuts,
    age,
    edu = uni_degree_percentage,
    img = log_immigrant_percentage,
    inc = log_income_per_capita,
    soc = log_global_friends,
    gen = gender
  ) %>%
  na.omit()

# categorical splits. Grouped by the NUTS3 unit, split the covariates into groups
covar_cat <- covar %>%
  group_by(NUTS3) %>%
  summarise(across(BID:soc, mean)) %>%
  ungroup() %>%
  mutate(across(BID:soc, function(x)ntile(x, 3) %>% as.factor()))
colnames(covar_cat) <- c('NUTS3', paste0(colnames(covar_cat)[-1], '_category'))

# join with the raw data
covar_joined <- covar %>% left_join(covar_cat)
covar_joined <- covar_joined %>% select(NUTS3:WID, pops, gen:soc_category)

# plot for all
long <- covar_joined %>%
  pivot_longer(gen:soc_category, names_to = 'covariate', values_to = 'category') %>%
  mutate(category = as.factor(category))

# aggregate across the groups
long_agg <- long %>%
  group_by(NUTS3, covariate, category) %>%
  summarise(across(BID:pops, mean))
long_agg$covariate <- str_remove_all(long_agg$covariate, '_category')

# make plots
wid <- long_agg %>%
  filter(!covariate %in% c('BID', 'pops', 'WID')) %>%
  ggplot(aes(pops, WID, group = category, colour = category)) +
  geom_point(size = 0.5) +
  geom_smooth(method = 'glm', size = 0.7) +
  facet_wrap(~ covariate) +
  labs(x = 'Population size (log base 10)', colour = 'Groups') +
  scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
  annotation_logticks(
    sides = 'b'
  )

bid <- long_agg %>%
  filter(!covariate %in% c('BID', 'pops', 'gen')) %>%
  ggplot(aes(pops, BID, group = category, colour = category)) +
  geom_point(size = 0.5) +
  geom_smooth(method = 'glm', size = 0.7) +
  facet_wrap(~ covariate) +
  labs(x = 'Population size (log base 10)', colour = 'Groups') +
  scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
  annotation_logticks(
    sides = 'b'
  ) + theme(legend.position = 'none')

# save the plot for SI figure
(joined_plot <- wid + bid + plot_layout(guide = 'collect'))
plot_save(joined_plot, 'SI/group_split_correlations', c(183, 150))
