#' Aggregate streams and map users to geolocations.
#' Raw stream and user information are not available as open access.

# load project wide packages and functions
source('utils.R')

countries <- c('fr', 'de', 'br')
data_dir <- 'PATH_TO_DIR'

################################################################################
## STREAM
################################################################################
# raw streaming data are pulled (not available as open access)
all_files <- list.files(file.path(data_dir, 'stream/anal'))
for (i in countries) {
  message(sprintf('Starting for %s', i))
  
  # find all the analysis stream files for country
  fls <- all_files[str_detect(all_files, i)]
  
  fl_paths <- file.path(data_dir, 'stream/anal', fls)
  
  agg_fls <- do.call(rbind, purrr::map(fl_paths, read.csv))
  
  # save the aggregated version
  agg_fls %>% saveRDS(file.path('data/private/stream/', paste0(i, '_stream_1month_LAN.rds')))
}


################################################################################
## USER
################################################################################
# all associated user data are pulled (not available as open access)
user <- file.path(data_dir, 'user/user_meta.csv') %>% read_csv()

# separate the user groups
fr_user <- user %>% filter(loc_country == 'FR')
br_user <- user %>% filter(loc_country == 'BR')
de_user <- user %>% filter(loc_country == 'DE')

# load regional mapping to using the custom 'map_users' function in 'utils.R'
fr_reg <- readRDS('data/census/fr/fr_gisco_muni_2020.rds')
br_reg <- readRDS('data/census/br/br_ibge_muni_2021.rds')
de_reg <- readRDS('data/census/de/de_gisco_muni_2020.rds')

# mapping users
fr_mapped <- map_users(fr_user, fr_reg)
fr_user_clean <- fr_mapped %>% left_join(fr_reg %>% as_tibble() %>% select(muni_code, muni_name, NUTS3, NUTS3_name))
fr_user_clean %>% saveRDS('data/private/user/fr_user.rds')

de_mapped <- map_users(de_user, de_reg)
de_user_clean <- de_mapped %>% left_join(de_reg %>% as_tibble() %>% select(muni_code, muni_name, NUTS3, NUTS3_name))
de_user_clean %>% saveRDS('data/private/user/de_user.rds')

br_mapped <- map_users(br_user, br_reg, CRS = 4674) # correct for the right CRS
br_user_clean <- br_mapped %>% left_join(br_reg %>% as_tibble() %>% select(muni_code, muni_name, state_code, state_name))
br_user_clean$muni_code <- as.character(br_user_clean$muni_code)
br_user_clean %>% saveRDS('data/private/user/br_user.rds')


################################################################################
## USER EMBEDDING
################################################################################
user_embedding <- file.path(data_dir, 'user/embedding') %>% load_batch(drop_loc_cols = F) 
user_embedding %>% write_rds('data/private/user_embedding.rds')


################################################################################
## MAKE REGION CODEBOOK
################################################################################
# make regional code book for each country
fr_region <- readRDS('data/census/fr/fr_insee_2022_meta.rds')
fr_region <- fr_region %>% 
  select(muni_code = insee_code, muni_name, state_code = NUTS3, state_name = NUTS3_name) %>% 
  cbind(state_type = 'NUTS3', country = 'France', country_code = 'FR') %>%
  mutate(regional_group = state_code, regional_group_name = state_name)

br_region <- readRDS('data/census/br/br_ibge_muni_2021.rds') %>% as_tibble()
br_region <- br_region %>% 
  select(muni_code, muni_name, state_code = state_code, state_name) %>% 
  cbind(state_type = 'state', country = 'Brazil', country_code = 'BR') %>%
  mutate(regional_group = muni_code, regional_group_name = muni_name)

de_region <- readRDS('data/census/de/de_gisco_muni_2020.rds') %>% as_tibble()
de_region <- de_region %>% 
  select(muni_code, muni_name, state_code = NUTS3, state_name = NUTS3_name) %>% 
  cbind(state_type = 'NUTS3', country = 'Germany', country_code = 'DE') %>%
  mutate(regional_group = state_code, regional_group_name = state_name)

# aggregate the entire regional codes and save
region_codebook <- do.call(rbind, list(fr_region, br_region, de_region))
region_codebook %>% write_csv('data/census/region_codebook.csv')

