#' Get municipalities level geographical boundaries and clean the data.
#' Outcome can be found at: 'data/census'

# load project wide packages and functions
source('utils.R')

################################################################################
## MUNICIPALITIES
################################################################################
# 1. get the geometry of municipalities in each country
# 2. extract the latitude and longitude as centroids
# 3. add nuts3 level info
nuts_lau <- read.csv('data/census/eurostat_2021_nuts3_lau_correspondence_table.csv', sep = ';')

# FRANCE
france_commune <- giscoR::gisco_get_lau(country = 'FR', year = '2020')
france_commune <- france_commune %>%
  select(
    country_code = CNTR_CODE,
    muni_code = LAU_ID,
    muni_name = LAU_NAME,
    muni_pop = POP_2020,
    muni_pop_dens = POP_DENS_2020,
    muni_area = AREA_KM2,
    geom = "_ogr_geometry_"
  )

france_commune_centroid <- france_commune %>%
  select(muni_code, geom) %>%
  sf::st_centroid()
france_commune_centroid$muni_lng <- sapply(france_commune_centroid$geom, function(x){unlist(x)[1]})
france_commune_centroid$muni_lat <- sapply(france_commune_centroid$geom, function(x){unlist(x)[2]})
france_commune_centroid <- france_commune_centroid %>% as_tibble() %>% select(muni_code, muni_lat, muni_lng)

# add the centroid points and the nuts code
france_clean <- france_commune %>% left_join(france_commune_centroid)
france_clean <- france_clean %>% left_join(nuts_lau)

# add nuts name
fr_nuts3 <- giscoR::gisco_get_nuts(nuts_level = 3, country = 'FR') %>%
  as_tibble() %>%
  select(NUTS3 = NUTS_ID, NUTS3_name = NUTS_NAME)

france_clean <- france_clean %>% left_join(fr_nuts3)
france_clean %>% saveRDS('data/census/fr/fr_gisco_muni_2020.rds')


# GERMANY
germany_commune <- giscoR::gisco_get_lau(country = 'DE', year = '2020')
germany_commune <- germany_commune %>%
  select(
    country_code = CNTR_CODE,
    muni_code = LAU_ID,
    muni_name = LAU_NAME,
    muni_pop = POP_2020,
    muni_pop_dens = POP_DENS_2020,
    muni_area = AREA_KM2,
    geom = "_ogr_geometry_"
  )

germany_commune_centroid <- germany_commune %>%
  select(muni_code, geom) %>%
  sf::st_centroid()
germany_commune_centroid$muni_lng <- sapply(germany_commune_centroid$geom, function(x){unlist(x)[1]})
germany_commune_centroid$muni_lat <- sapply(germany_commune_centroid$geom, function(x){unlist(x)[2]})
germany_commune_centroid <- germany_commune_centroid %>% as_tibble() %>% select(muni_code, muni_lat, muni_lng)

# add the centroid points and the nuts code
germany_clean <- germany_commune %>% left_join(germany_commune_centroid)
germany_clean <- germany_clean %>% left_join(nuts_lau)

# add nuts name
de_nuts3 <- giscoR::gisco_get_nuts(nuts_level = 3, country = 'DE') %>%
  as_tibble() %>%
  select(NUTS3 = NUTS_ID, NUTS3_name = NUTS_NAME)

germany_clean <- germany_clean %>% left_join(de_nuts3)
germany_clean %>% saveRDS('data/census/de/de_gisco_muni_2020.rds')


# BRAZIL
brazil_commune <- geobr::read_municipality(year = 2021)
brazil_commune <- brazil_commune %>%
  cbind(country_code = 'BR') %>%
  select(
    country_code,
    muni_code = code_muni,
    muni_name = name_muni,
    state_code = code_state,
    state_name = name_state,
    region_code = code_region,
    region_name = name_region,
    geom
  )

brazil_commune_centroid <- brazil_commune %>%
  select(muni_code, geom) %>%
  sf::st_centroid()
brazil_commune_centroid$muni_lng <- sapply(brazil_commune_centroid$geom, function(x){unlist(x)[1]})
brazil_commune_centroid$muni_lat <- sapply(brazil_commune_centroid$geom, function(x){unlist(x)[2]})
brazil_commune_centroid <- brazil_commune_centroid %>% as_tibble() %>% select(muni_code, muni_lat, muni_lng)

# add the centroid points and the nuts code
brazil_clean <- brazil_commune %>% left_join(brazil_commune_centroid)

brazil_clean %>% saveRDS('data/census/br/br_ibge_muni_2021.rds')


