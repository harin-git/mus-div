#' Explore WID as function of age.
#' Raw user information is not available as open access.
#' Aggregated bootstrapped data is available at: 'data/diversity'

# load all necessary packages and functions
source('utils.R')

################################################################################
## PREPARATION
################################################################################
# get raw user data
fr_user <- readRDS('data/private/user/fr_user.rds')
br_user <- readRDS('data/private/user/br_user.rds')
de_user <- readRDS('data/private/user/de_user.rds')

joined_user <- do.call(bind_rows, list(fr_user, br_user, de_user))

# clean user data by age and outlier GS-scores
joined_user <- joined_user %>%
  select(study_user_id, country, age, WID = inv_gs_score, n_streams) %>%
  filter(age >= 16, age <= 65) %>%
  filter(country %in% c('FR', 'BR', 'DE')) %>%
  filter(!is.na(WID), WID < 75) %>%
  mutate(age_category = case_when(
    age > 15 & age <= 20 ~ 15,
    age > 20 & age <= 25 ~ 20,
    age > 25 & age <= 30 ~ 25,
    age > 30 & age <= 35 ~ 30,
    age > 35 & age <= 40 ~ 35,
    age > 40 & age <= 45 ~ 40,
    age > 45 & age <= 50 ~ 45,
    age > 50 & age <= 55 ~ 50,
    age > 55 & age <= 60 ~ 55,
    age > 60 & age <= 65 ~ 60
  ) %>% as.factor())

################################################################################
## BOOTSTRAPPED RESULTS 
################################################################################
# bootstrap by user category
boot_age = list()
for (i in 1:1000) {
  boot_age[[i]] <- joined_user %>%
    group_by(country, age_category) %>%
    sample_n(n(), replace = TRUE) %>%
    summarise(WID = mean(WID)) %>%
    cbind(boot = i)
  
  print(i)
}
boot_age <- do.call(rbind, boot_age)
boot_age %>% write_rds('bootstrap_output/WID_age_B1000_boot.rds')

# summarise the results
agg_country <- boot_age %>%
  group_by(country, age_category) %>%
  reframe(get_boot_mean_ci(WID, 'boot'))

# save the aggregated data
agg_country %>% write_csv('data/diversity/WID_and_age.csv')

# plot the trajectories
agg_country$country <- factor(agg_country$country, 
                              levels = c('FR', 'BR', 'DE'), 
                              labels = c('France', 'Brazil', 'Germany'))

(agg_country_p <- agg_country %>%
  ggplot(aes(age_category, boot_m, ymin = boot_lower_ci, ymax = boot_upper_ci, 
             group = country,
             colour = I(ifelse(
               country == 'France',
               FR_COL1,
               ifelse(country == 'Brazil', BR_COL1, DE_COL1)
             ))
             )) +
  geom_point() +
  geom_errorbar() +
  geom_line() +
  theme(strip.text = element_blank()) +
  facet_wrap(~ country, scale = 'free_y') +
  labs(x = 'Age', y = 'WID'))

agg_country_p %>% plot_save('main/WID_w_age', size = c(183, 50))


################################################################################
## CONTROLLING FOR AMOUNT OF STREAMS
################################################################################
control_stream_amount <- joined_user %>%
  ungroup() %>%
  mutate(stream_quantile = ntile(n_streams, 3) %>% as.factor())

control_stream_amount_agg <- control_stream_amount %>%
  group_by(country, age_category, stream_quantile) %>%
  summarise(m = mean(WID),
            ci = margin(WID))

control_stream_amount_agg$country <- factor(control_stream_amount_agg$country, 
                              levels = c('FR', 'BR', 'DE'), 
                              labels = c('France', 'Brazil', 'Germany'))

(stream_control_p <- control_stream_amount_agg %>%
  ggplot(aes(age_category, m, ymin = m - ci, ymax = m + ci,
             shape = stream_quantile, 
             group = interaction(country, stream_quantile),
             colour = I(ifelse(
               country == 'France',
               FR_COL1,
               ifelse(country == 'Brazil', BR_COL1, DE_COL1)
             ))
  )) +
  geom_pointrange() +
  geom_line() +
  facet_wrap(~ country, scale = 'free_y') +
  labs(x = 'Age', y = 'WID', shape = 'Stream amount category'))

stream_control_p %>% plot_save('SI/stream_control_WID_w_age', size = c(183, 80))




