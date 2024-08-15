library(sf)
library(spdep)
library(tmap)

################################################################################
# MORAN'S I TEST
################################################################################
# define country
country <- 'de'

# load data
div  <- read.csv(sprintf('data/diversity/%s_diversity.csv', stringr::str_to_upper(country))) %>% 
  filter(metric %in% c('song_div_q1', 'ind_div'))
div <- div %>% select(regional_group, metric, boot_m, n_users) %>%
  pivot_wider(names_from = metric, values_from = boot_m) %>%
  rename(BID = song_div_q1,
         WID = ind_div)

map <- read_rds(sprintf('data/census/%s/%s_map.rds', country, country))
if(country == 'br'){
  map <- map %>% rename(regional_group = muni_code)
  map <- map %>% rename(geometry = geom)
} else {
  map <- map %>% rename(regional_group = NUTS_ID)
}

# join the map and diversity data
joined <- map %>% left_join(div) %>% na.omit()

# assign neighbours
nb <- poly2nb(joined, queen=TRUE)

# assign equal weights to the neighbours
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# # moran I's statistics
# I <- moran(joined$BID, lw, length(nb), Szero(lw))[1]

# MC method for hypothesis testing for both diversity measures
message('Moran I for BID')
mc_bid <- moran.mc(joined$BID, lw, nsim=999, alternative="greater")
print(mc_bid)
plot(mc_bid, main = 'Moran I for BID')


message('Moran I for WID')
mc_wid <- moran.mc(joined$WID, lw, nsim=999, alternative="greater")
print(mc_wid)
plot(mc_wid, main = 'Moran I for WID')


################################################################################
# NUTS LEVEL 2
################################################################################
# regroup NUTS by level 2
div2 <- div %>%
  mutate(regional_group = str_sub(regional_group, 1, 4)) %>%
  group_by(regional_group) %>%
  summarise(BID = mean(BID),
            WID = mean(WID),
            n_users = sum(n_users))

# test for correlations
cor.test(div2$BID, log10(div2$n_users))
cor.test(div2$WID, log10(div2$n_users))


################################################################################
# DETRENDING ANALYSIS
################################################################################
# transform the CRS into meter
detrend <- st_transform(joined, crs = 3857) # EPSG:3857 is a common CRS in meters

# Create a buffer of 100 km around each point
detrend_transformed <- st_buffer(detrend$geometry, dist = 100000) %>% st_geometry() # 100,000 meters = 100 km

# work out which point is within each circle
overlaps <- st_within(y = detrend_transformed, x = detrend)

# For each buffer, calculate the mean WID and BID of the points within it, excluding the focal point
detrend$mean_WID <- sapply(seq_along(overlaps), function(i) mean(detrend$WID[overlaps[[i]][-1]], na.rm = TRUE))
detrend$mean_BID <- sapply(seq_along(overlaps), function(i) mean(detrend$BID[overlaps[[i]][-1]], na.rm = TRUE))

# each value minus the mean
detrend$detrend_WID <- detrend$WID - detrend$mean_WID
detrend$detrend_BID <- detrend$BID - detrend$mean_BID

# NAN then replace with 0 as there are no neighbours other than focal itself
detrend$detrend_WID[is.na(detrend$detrend_WID)] <- 0
detrend$detrend_BID[is.na(detrend$detrend_BID)] <- 0

# test for correlations
message('BID detrend')
cor.test(log10(detrend$n_users), detrend$detrend_BID)
message('WID detrend')
cor.test(log10(detrend$n_users), detrend$detrend_WID)

# covariate analysis
lm(log10(n_users) ~ WID + mean_WID, data = detrend) %>% summary()
lm(log10(n_users) ~ BID + mean_BID, data = detrend) %>% summary()


