#' Compute the bootstrap estimates for BID and WID.
#' Raw stream and user information are not available as open access.
#' Aggregated bootstrapped data is available at: 'data/diversity'

# load project wide packages and functions
source('utils.R')

# Hyper param
country <- 'FR' # define country as FR, DE, BR
min_user_per_region <- 200 # to ensure anonymity at the regional aggregate
max_geo_loc <- 10 # users with no more than 10 unique IP address locations
N_BOOT <- 1000 # number of bootstraps

################################################################################
## DATA PREPARATION
################################################################################
user_data <- switch(country,
                    FR = readRDS('data/private/user/fr_user.rds') %>% rename(regional_group = NUTS3),
                    DE = readRDS('data/private/user/de_user.rds') %>% rename(regional_group = NUTS3),
                    BR = readRDS('data/private/user/br_user.rds') %>% rename(regional_group = muni_code))

# filter only regions where more than 200 people can be used for aggregation
all_regions <- user_data$regional_group %>% unique()

# very high values of GS score is noise. Ambiguous GS-scores are coded 100
select_region <- user_data %>%
  filter(!is.na(inv_gs_score), inv_gs_score < 75, n_geo_loc < max_geo_loc) %>%
  group_by(regional_group) %>%
  filter(n() >= min_user_per_region)
select_region <- select_region$regional_group %>% unique()

message(sprintf('Out of the %s initial regional groups, %s regions were included as others had too little sample size (%s percent kept)',
                length(all_regions), length(select_region), round(length(select_region) / length(all_regions) *100)
))

# make a cleaned user set
user_clean <- user_data %>%
  filter(regional_group %in% select_region, n_geo_loc < max_geo_loc) %>%
  select(study_user_id, regional_group, n_deezer_clusters, inv_gs_score, mean_age_songs)

# make a population size by log10
user_n <- user_clean %>%
  group_by(regional_group) %>%
  summarise(n_users = n()) %>%
  ungroup() %>%
  mutate(user_pop_size_log10 = log10(n_users))

# save the cleaned population size for each region
user_n %>% saveRDS(sprintf('data_output/%s_1month_user_population.rds', country))

# get stream data
stream_data <- switch(country,
                      FR = readRDS('data/private/stream/fr_stream_1month_LAN.rds'),
                      DE = readRDS('data/private/stream/de_stream_1month_LAN.rds'),
                      BR = readRDS('data/private/stream/br_stream_1month_LAN.rds'))

stream_clean <- user_clean %>%
  select(user_study_id, regional_group) %>%
  left_join(stream_data)

# add song information
song_data <- readRDS('data/private/song/song_meta.rds')
song_data_simple <- song_data %>% 
  select(product_id, artist_id, raw_genre) %>%
  filter(product_id %in% stream_clean$product_id)
song_data_simple <- song_data_simple %>%
  group_by(product_id) %>%
  mutate(selected_genre = grab_random(raw_genre)) %>%
  ungroup() %>%
  select(-raw_genre)

# join song information to stream data
stream_clean <- stream_clean %>% left_join(song_data_simple)

################################################################################
## BOOT STRAPPING
################################################################################
regional_stat <- function(stream_data, user_data, n_boot = 10, n_stream_sample = 10000, 
                          n_user_sample = 100, replace_boot = F){
  # make different groupings depending on whether to do the split or not
  message('Grouping data...')
  grouped_stream <- stream_data %>% group_by(regional_group)
  grouped_user <- user_data %>% filter(!is.na(inv_gs_score), inv_gs_score < 75) %>% group_by(regional_group)
  
  
  # bootstrap across same number of samples
  message(sprintf('Starting %s bootstraps', n_boot))
  output = list()
  for (i in 1:n_boot) {
    # BID: Hill's number diversity and Gini-coefficient
    # q = 0 ~ richness
    # q = 1 ~ shanon-entropy
    # q = 2 ~ simpson's diversity index
    
    regional_div <- grouped_stream %>%
      sample_n(n_stream_sample, replace = replace_boot) %>%
      summarise(
        # by song
        song_div_q0 = hill(table(product_id), q = 0), 
        song_div_q1 = hill(table(product_id), q = 1), 
        song_div_q2 = hill(table(product_id), q = 2), 
        song_gini = DescTools::Gini(table(product_id)),
        # by artist
        artist_div_q0 = hill(table(artist_id), q = 0), 
        artist_div_q1 = hill(table(artist_id), q = 1), 
        artist_div_q2 = hill(table(artist_id), q = 2), 
        artist_gini = DescTools::Gini(table(artist_id)),
        # by genre
        genre_div_q0 = hill(table(selected_genre), q = 0), 
        genre_div_q1 = hill(table(selected_genre), q = 1), 
        genre_div_q2 = hill(table(selected_genre), q = 2), 
        genre_gini = DescTools::Gini(table(selected_genre))
      )
    
    # WID: Hill's number diversity and Gini-coefficient
    user_div <- grouped_user %>%
      sample_n(n_user_sample, replace = replace_boot) %>%
      summarise(ind_div = mean(inv_gs_score),
                song_age = mean(mean_age_songs))
    
    # aggregate the output and save
    output[[i]] <- cbind(boot = i, regional_div) %>% left_join(user_div, by = join_by(regional_group))
    
    print(sprintf('%s out of %s bootstraps', i, n_boot))
    
  }
  output <- do.call(rbind, output)
}

# save the bootstrap outputs
boot_stat <- regional_stat(stream_clean,
                           user_clean,
                           n_boot = N_BOOT,
                           replace_boot = FALSE)
boot_stat %>% saveRDS(file.path('bootstrap_output', sprintf('%s_1month_B%s_boot.rds', country, N_BOOT)))


# aggregate across the bootstraps for summary statistics
agg_stat <- boot_stat %>%
  pivot_longer(cols = song_div_q0:song_age, names_to = 'metric') %>%
  group_by(regional_group, metric) %>%
  reframe(get_boot_mean_ci(value, 'boot'))

# add population size
agg_stat <- agg_stat %>% left_join(user_n)
agg_stat %>% write_csv(file.path('data/diversity', sprintf('%s_diversity.csv', country)))

# Report mean and CI stats across the regions in SI
agg_si <- agg_stat %>%
  group_by(metric) %>%
  reframe(get_t_independent(boot_m))
agg_si
