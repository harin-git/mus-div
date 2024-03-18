#' Clean and explore the distribution of confounders included in the DAG model.
#' The output of this script serve as the input data for the DAG model.
#' Raw user information is not available as open access.

# load project wide packages and functions
source('utils.R')

# helper function to make small histograms
make_small_histogram <- function(df, var_name){
  (df %>%
     ggplot(aes(!!sym(var_name))) +
     geom_histogram(bins = 20) +
     theme_void()) %>%
    plot_save(plot_name = paste0('SI/', var_name, '_distribution_hist'), size = c(50, 30), pdf = FALSE)
}

# additionally 'test_norm' function from utils.R is used to visualise the distribution

################################################################################
## PREPARTION
################################################################################
fr_users <- readRDS('data/private/user/fr_user.rds')  # all French users
fr_pop <- readRDS('data_output/FR_1month_user_population.rds') # population size
fr_census <- readRDS('data/census/fr/fr_insee_2022_meta.rds') # census data
fr_venues <- read.csv('data/census/fr/fr_music_venues.csv') # music venues
fr_bid_wid <- read.csv('data/diversity/FR_diversity.csv') # computed regional BID and WID
facebook_friends <- read.csv('data/census/fr/fr_social_connections.csv') # facebook social connectivity index data

# define the number of quantile to split the data. 
# Having more quantile comes with more granularity but loses the balance across the groups in DAG
num_quantiles <- 3

################################################################################
## USER INFO & WID
################################################################################
users_clean <- fr_users %>%
  select(study_user_id, muni_code, muni_name, NUTS3, NUTS3_name, age, gender, WID = inv_gs_score) %>%
  # filter only the users with gender information and age
  filter(age >= 18, age <= 65, gender %in% c('M', 'F'), WID < 75, WID > 0) %>%
  mutate(age = as.numeric(age),
         gender = as.factor(gender),
         WID = as.numeric(WID)) %>%
  # make WID and age at the NUTS level for comparison with BID
  group_by(NUTS3) %>%
  mutate(WID_nuts = mean(WID),
         age_nuts = mean(age))

# age
users_clean$age %>% test_norm('Age')
make_small_histogram(users_clean, 'age')

# WID
users_clean$WID_nuts %>% test_norm('WID')
make_small_histogram(users_clean, 'WID_nuts')

################################################################################
## BID
################################################################################
BID <- fr_bid_wid %>%
  filter(metric == 'song_div_q1') %>%
  select(NUTS3 = regional_group, BID_nuts = boot_m) %>%
  mutate(log_BID_nuts = log10(BID_nuts),
         BID_category = ntile(log_BID_nuts, num_quantiles) %>% as.factor())

BID$BID_nuts %>% test_norm('BID')
BID$log_BID_nuts %>% test_norm('log BID')

make_small_histogram(BID, 'log_BID_nuts')


################################################################################
## POPULATION SIZE
################################################################################
# add population size category derived from the population group
pop_clean <- fr_pop %>%
  select(NUTS3 = regional_group, log_pop_size_nuts = user_pop_size_log10, pop_size_nuts = n_users) %>%
  mutate(pop_size_category = ntile(log_pop_size_nuts, num_quantiles) %>% as.factor())

pop_clean$pop_size_nuts %>% test_norm('Region population size')
pop_clean$log_pop_size_nuts %>% test_norm('Log region population size')

make_small_histogram(pop_clean, 'log_pop_size_nuts')

# check population split relative to census. Report in SI
fr_nuts <- read.csv('data/census/fr/fr_nuts3_meta.csv')
check_pop <- pop_clean %>% left_join(fr_nuts %>% select(NUTS3, population))
check_pop %>% 
  group_by(pop_size_category) %>% 
  summarise(median = median(population),
            min = min(population),
            max = max(population))


################################################################################
## CENSUS
################################################################################
# clean census information from INSEE
census_clean <- replace(fr_census, fr_census == 0, 0.01) # make 0 really small number for log transformation of 0

# do log transformation
census_clean <- census_clean %>%
  select(muni_code = insee_code, muni_pop_size = population,
         income_per_capita, immigrant_percentage, uni_degree_percentage) %>%
  mutate(across(muni_pop_size:uni_degree_percentage, log10, .names = "log_{.col}"))

census_clean$log_muni_pop_size %>% test_norm('Log municipalities population')
census_clean$log_income_per_capita %>% test_norm('Log income per capita')
census_clean$log_immigrant_percentage %>% test_norm('Log immigrant percentage')
census_clean$uni_degree_percentage %>% test_norm('Uni graduate percentage')

make_small_histogram(census_clean, 'log_income_per_capita')
make_small_histogram(census_clean, 'log_immigrant_percentage')
make_small_histogram(census_clean, 'uni_degree_percentage')


#################################################################################
## MUSICAL VENUES
################################################################################
# count the number of venues in nuts (too little data if commune level)
venue_clean <- fr_venues %>%
  group_by(NUTS3) %>%
  summarise(n_music_venues = n()) %>%
  ungroup() %>%
  mutate(log_n_music_venues = log10(n_music_venues))
  

venue_clean$n_music_venues %>% test_norm('N musical venues')
venue_clean$log_n_music_venues %>% test_norm('Log N musical venues')

make_small_histogram(venue_clean, 'log_n_music_venues')


#################################################################################
## GLOBAL FRIENDS
################################################################################
friends <- facebook_friends %>%
  select(NUTS3, global_friends) %>%
  mutate(log_global_friends = log10(global_friends)) 

friends$global_friends %>% test_norm('Global friends')
friends$log_global_friends %>% test_norm('Log global friends')

make_small_histogram(friends, 'log_global_friends')


################################################################################
## OUTPUT
################################################################################
# aggregate all to make dataset to run DAG models
output <- users_clean %>%
  left_join(BID) %>%
  left_join(pop_clean) %>%
  left_join(census_clean) %>%
  left_join(venue_clean) %>%
  left_join(friends)

# see the extent of missing data in each variable
non_factor_vars <- output %>%
  select(age:log_global_friends) %>%
  select_if(~ !is.factor(.)) 

non_factor_vars %>%
  finalfit::ff_glimpse()

# largest missing data is for commune level uni degree percentage (17%)
# this is all coming from Paris (FR101) not having commune data
output[is.na(output$uni_degree_percentage), ]$NUTS3 %>% unique()

# draw the data at the nuts level
nuts_edu <- read.csv('../../france_political_conflict_data/diplomas/diplomesdepartements.csv') %>%
  select(dep, nomdep, pbac2022)

paris_nuts_edu <- nuts_edu %>% filter(nomdep == 'PARIS')
paris_nuts_edu <- paris_nuts_edu$pbac2022

# replace all the missing ones with PARIS value
output <- output %>%
  mutate(
    uni_degree_percentage = ifelse(
      NUTS3 == 'FR101',
      paris_nuts_edu,
      uni_degree_percentage
    ),
    log_uni_degree_percentage = log10(uni_degree_percentage)
  )

# test for missing data again
non_factor_vars <- output %>%
  select(age:log_global_friends) %>%
  select_if(~ !is.factor(.)) 

non_factor_vars %>%
  finalfit::ff_glimpse()

# only few missing (N = 17) and so dropped
output %>% na.omit() %>% saveRDS('data_output/dag_data.rds')




