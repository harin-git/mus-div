#' Map approximate users by their geolocations on the geographical map
#' Raw user information is not available as open access.

# load project wide packages and functions
source('utils.R')

# define CRS
CRS = 4326

################################################################################
## FRANCE
################################################################################
fr_nuts <- readRDS('data/census/fr/fr_map.rds') 
fr_users <- readRDS('data/private/user/fr_user.rds') %>%
  select(study_user_id, lat = loc_lat, lng = loc_long)
fr_coord_conv <- fr_users %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs = CRS)

fr_user_map <- ggplot() +
  geom_sf(data = fr_nuts, fill = FR_COL1, color="gray", size=.01, show.legend = FALSE) +
  geom_sf(data = fr_coord_conv, size = 0.05, alpha = 0.05, colour = FR_COL2)
fr_user_map %>% plot_save('main/fr_user_map', c(100, 100), pdf = F)


################################################################################
## GERMANY
################################################################################
de_nuts <- readRDS('data/census/de/de_map.rds') 
de_users <- readRDS('data/private/user/de_user.rds') %>%
  select(study_user_id, lat = loc_lat, lng = loc_long)
de_coord_conv <- de_users %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs = CRS)

de_user_map <- ggplot() +
  geom_sf(data = de_nuts, fill = DE_COL1, color="gray", size=.01, show.legend = FALSE) +
  geom_sf(data = de_coord_conv, size = 0.05, alpha = 0.05, colour = DE_COL2)
de_user_map %>% plot_save('main/de_user_map', c(100, 100), pdf = F)


################################################################################
## BRAZIL
################################################################################
br_state <- readRDS('data/census/br/br_map.rds')
br_users <- readRDS('data/private/user/br_user.rds') %>% 
  select(study_user_id, lat = loc_lat, lng = loc_long)
br_coord_conv <- br_users %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs = CRS)

br_user_map <- ggplot() +
  geom_sf(data = br_state, fill = BR_COL1, color="gray", size=.01, show.legend = FALSE) +
  geom_sf(data = br_coord_conv, size = 0.05, alpha = 0.05, colour = BR_COL2)
br_user_map %>% plot_save('main/br_user_map', c(100, 100), pdf = F)

