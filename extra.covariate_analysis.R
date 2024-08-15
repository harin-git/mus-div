#' Covariate analysis using all demographic factors (i.e., throwing everything into the sink approach)
#' Raw user information is not available as open access.
#' Aggregated bootstrapped data is available at: 'data/dag'

# load project wide packages and functions
source('utils.R')

# load dag and census data
dag_data <- readRDS('data_output/dag_data.rds')

census_data <- read.csv('data/census/fr/fr_nuts3_meta.csv')
dag_data_quantile <- census_data %>% left_join(dag_data %>% distinct(NUTS3, pop_size_category))
dag_data_quantile %>%
  group_by(pop_size_category) %>%
  summarise(median = median(population))

# make dataframe to use for the model
dag_var <- dag_data %>% 
  ungroup() %>%
  select(
    NUTS3, 
    BID = BID_nuts,
    WID = WID_nuts,
    WID_raw = WID,
    mus = log_n_music_venues,
    pops = log_pop_size_nuts,
    age,
    age_nuts,
    edu = uni_degree_percentage,
    img = log_immigrant_percentage,
    inc = log_income_per_capita,
    soc = log_global_friends,
    alg = algo_nuts,
    gen = gender,
    pop_category = pop_size_category
  ) %>%
  na.omit() %>%
  mutate(across(BID:soc, scale)) # Z-score all the continuous variables

# Report stats on how all demographic factors explain BID and WID in each population quantiles
# Population quantile = 1 (small), 2 (medium), 3 (large)
lm(BID ~ mus + age_nuts + edu + img + inc + soc + gen + alg, data = dag_var %>% filter(pop_category == 1)) %>% summary()
lm(BID ~ mus + age_nuts + edu + img + inc + soc + gen + alg, data = dag_var %>% filter(pop_category == 2)) %>% summary()
lm(BID ~ mus + age_nuts + edu + img + inc + soc + gen + alg, data = dag_var %>% filter(pop_category == 3)) %>% summary()

lm(WID ~ mus + age_nuts + edu + img + inc + soc + gen + alg, data = dag_var %>% filter(pop_category == 1)) %>% summary()
lm(WID ~ mus + age_nuts + edu + img + inc + soc + gen + alg, data = dag_var %>% filter(pop_category == 2)) %>% summary()
lm(WID ~ mus + age_nuts + edu + img + inc + soc + gen + alg, data = dag_var %>% filter(pop_category == 3)) %>% summary()
