#' Bootstrapped regional means of BID and WID as function of population size

# load project wide packages and functions
source('utils.R')

################################################################################
## DATA PREPARATION
################################################################################
N_BOOT <- 1000

# load diversity data
fr_div <- readRDS(file.path('bootstrap_output', sprintf('fr_1month_B%s_boot.rds', N_BOOT))) %>% cbind(country = 'France')
br_div <- readRDS(file.path('bootstrap_output', sprintf('br_1month_B%s_boot.rds', N_BOOT))) %>% cbind(country = 'Brazil')
de_div <- readRDS(file.path('bootstrap_output', sprintf('de_1month_B%s_boot.rds', N_BOOT))) %>% cbind(country = 'Germany')

joined_div <- do.call(bind_rows, list(fr_div, br_div, de_div))

# load user population size data
fr_pop <- readRDS(file.path('data_output', 'FR_1month_user_population.rds')) %>% cbind(country = 'France')
br_pop <- readRDS(file.path('data_output', 'BR_1month_user_population.rds')) %>% cbind(country = 'Brazil')
de_pop <- readRDS(file.path('data_output', 'DE_1month_user_population.rds')) %>% cbind(country = 'Germany')

joined_pop <- do.call(bind_rows, list(fr_pop, br_pop, de_pop))

################################################################################
## AGGREGATED MEAN BY REGION
################################################################################
agg_stat <- joined_div %>%
  group_by(country, regional_group) %>%
  reframe(BID = song_div_q1, WID = ind_div) %>% # choose which div metrics to use
  pivot_longer(BID:WID, names_to = 'metric') %>%
  group_by(country, regional_group, metric) %>%
  reframe(get_boot_mean_ci(value, 'boot')) %>%
  left_join(joined_pop) %>%
  ungroup()

agg_stat$country <- factor(agg_stat$country, levels = c('France', 'Brazil', 'Germany'))


################################################################################
## GAM FITTING
################################################################################
# 'gam_fit' custom function from 'utils.R' is used to fit GAM of each bootstrap
gam_BID <- joined_div %>%
  left_join(joined_pop) %>%
  group_by(country, boot) %>%
  reframe(gam_fit(user_pop_size_log10, song_div_q1)) %>%
  ungroup() %>%
  rename(user_pop_size_log10 = x, BID = y)

gam_WID <- joined_div %>%
  left_join(joined_pop) %>%
  group_by(country, boot) %>%
  reframe(gam_fit(user_pop_size_log10, ind_div)) %>%
  ungroup() %>%
  rename(user_pop_size_log10 = x, WID = y)

gam_join <- left_join(gam_BID, gam_WID) %>% 
  pivot_longer(BID:WID)
gam_join$country <- factor(gam_join$country, levels = c('France', 'Brazil', 'Germany'))

# get means and confidence of the GAM
gam_agg <- gam_join %>%
  group_by(country, user_pop_size_log10, name) %>%
  reframe(get_boot_mean_ci(value, 'gam'))

################################################################################
## PLOT DIVERSITY AS FUNCTION OF POP SIZE
################################################################################
plot_diversity <- function(plot_type = c('WID', 'BID')){
  
  # diversity over continuous population
  (p <- ggplot(data = NULL, # define global aesthetics
               aes(
                 x = user_pop_size_log10,
                 colour = I(ifelse(
                    country == 'France',
                    FR_COL1,
                    ifelse(country == 'Brazil', BR_COL1, DE_COL1))),
                  fill = I(ifelse(
                    country == 'France',
                    FR_COL1,
                    ifelse(country == 'Brazil', BR_COL1, DE_COL1)))
                 )) +
    # Plot the bootstrapped mean values
    geom_point(data = agg_stat %>% filter(metric == plot_type),
               aes(y = boot_m)) +
    # # Plot the mean of GAM fits of the bootstraps
    geom_line(data = gam_agg %>% filter(name == plot_type),
              aes(y = gam_m), alpha = 1) +
    # CI ribbon around the GAM mean
    geom_ribbon(data = gam_agg %>% filter(name == plot_type),
                aes(y = gam_m, ymin = gam_lower_ci, ymax = gam_upper_ci),
                size = 0, alpha = 0.2) +
    # Formatting
    facet_wrap(~ country, scales = ifelse(plot_type == 'WID', 'fixed', 'free_y')) +
    scale_x_continuous(labels = math_format(10 ^ .x), breaks = c(3, 4, 5)) +
    annotation_logticks(
      sides = 'b'
    ) +
    labs(x = 'Population size', y = plot_type) +
    theme(legend.position = 'none',
          strip.background = element_blank(),
          strip.text = element_blank()))
  
  message('Saving plot')
  # save plot
  switch (plot_type,
          'WID' = plot_save(p, plot_name = 'main/WID_w_pop_size', c(183, 60)),
          'BID' = plot_save(p, plot_name = 'main/BID_w_pop_size', c(183, 60))
  )

}

# make plots and save
plot_diversity('WID')
plot_diversity('BID')


# STATS: bootstrapped correlations for SI reporting
cor_boot <- joined_div %>%
  left_join(joined_pop) %>%
  select(country, boot, user_pop_size_log10, song = song_div_q1, artist = artist_div_q1, genre = genre_div_q1, ind = ind_div) %>%
  group_by(country, boot) %>%
  summarise(across(
    c(song:ind),
    list(
      pearson = ~cor(.x, user_pop_size_log10, method = 'pearson'),
      spearman = ~cor(.x, user_pop_size_log10, method = 'spearman')
    )
  )) %>%
  ungroup() 

cor_boot_agg <- cor_boot %>%
  pivot_longer(song_pearson:ind_spearman, names_to = 'metric') %>%
  group_by(country, metric) %>%
  reframe(get_boot_mean_ci(value, 'boot'))

cor_boot_agg <- cor_boot_agg %>%
  mutate_if(is.numeric, ~signif(., 2)) %>%
  mutate(country = factor(country, levels = c('France', 'Brazil', 'Germany'))) %>%
  arrange(country)

print(cor_boot_agg)


# STATS: report 500,000 (urban) vs. 100,000 (rural) region differences
census_data <- read.csv('data/census/fr/fr_nuts3_meta.csv') %>% ungroup() 
census_data <- census_data %>% 
  select(regional_group = NUTS3, population) %>%
  mutate(region_category = ifelse(population >= 500000, 'urban', 'rural'))

census_join <- joined_div %>% left_join(census_data)

census_agg <- census_join %>%
  filter(!is.na(region_category)) %>%
  group_by(boot, region_category) %>%
  summarise(bid = mean(song_div_q1),
            wid = mean(ind_div))

census_diff <- census_agg %>%
  group_by(boot) %>%
  summarise(bid_diff = (bid[2] - bid[1]) / bid[1],
            wid_diff = (wid[2] - wid[1]) / wid[1]
  ) %>%
  ungroup() 

census_diff_agg <- census_diff %>%
  pivot_longer(bid_diff:wid_diff) %>%
  mutate(value = value * 100) %>%
  group_by(name) %>%
  summarise(m = mean(value),
            lower_ci = quantile(value, 0.025),
            upper_ci = quantile(value, 0.975))

print(census_diff_agg)

census_diff_cohen <- census_join %>%
  filter(!is.na(region_category)) %>%
  select(boot, region_category, bid = song_div_q1, wid = ind_div) %>%
  pivot_wider(names_from = region_category, values_from = c(bid, wid))
census_diff_cohen <- census_diff_cohen %>% 
  group_by(boot) %>%
  summarise(bid = get_t_stat(bid_rural %>% unlist(), bid_urban %>% unlist()),
            wid = get_t_stat(wid_rural %>% unlist(), wid_urban %>% unlist())
  ) 

census_diff_cohen$bid$cohensD %>% mean()
census_diff_cohen$bid$cohensD %>% quantile(c(0.025, 0.975))

census_diff_cohen$wid$cohensD %>% mean()
census_diff_cohen$wid$cohensD %>% quantile(c(0.025, 0.975))



################################################################################
## CROSS-CULTURAL DIFFERENCES
################################################################################
cross_cultural_difference <- function(plot_type = c('WID', 'BID')){
  # group level differences
  raw_group_points <- agg_stat %>%
    filter(metric == plot_type) %>%
    ungroup()
  
  (p <- raw_group_points %>%
    ggplot(aes(reorder(country, - boot_m), boot_m, 
               fill = I(ifelse(
                 country == 'France',
                 FR_COL1,
                 ifelse(country == 'Brazil', BR_COL1, DE_COL1)
               )))) +
    geom_boxplot(outlier.alpha = 0) +
    labs(x = '', 
         y = '') +
    scale_y_continuous(n.breaks = 4) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()))
  
  # save plot
  switch (plot_type,
          'WID' = plot_save(p, plot_name = 'main/WID_group_diff', size = c(50, 50)),
          'BID' = plot_save(p, plot_name = 'main/BID_group_diff', size = c(50, 50))
  )
  
}

cross_cultural_difference('WID')
cross_cultural_difference('BID')


## ANOVA and post hoc t-tests for the cross-cultural differences
plot_type <- 'BID'
agg_sel <- agg_stat %>%
  filter(metric == plot_type) %>%
  select(country, boot_m)

agg_sel %>%
  group_by(country) %>%
  reframe(get_t_independent(boot_m))

# perform anova to see group level differences
aov_model <- aov(boot_m ~ country, data = agg_sel)
aov_model %>% summary()

# post hoc pair-wise and effect size
TukeyHSD(aov_model)

agg_sel %>%
  pivot_wider(names_from = country, values_from = boot_m) %>%
  reframe(get_t_stat(Brazil %>% unlist(), France %>% unlist()))

################################################################################
## DIFFERENT TYPES OF BID: replicating for genres and artists
################################################################################
# split the population group by small vs. large
joined_pop_split <- joined_pop %>%
  group_by(country) %>%
  mutate(region_size = ntile(n_users, 3),
         region_size = case_match(region_size,
                    3 ~ 'L',
                    2 ~ 'M',
                    1 ~ 'S') %>% factor(levels = c('S', 'M', 'L'))) 

pop_split_agg <- joined_div %>%
  left_join(joined_pop_split) %>%
  group_by(country) %>%
  mutate(across(c(song_div_q1, artist_div_q1, genre_div_q1), scale)) %>%
  pivot_longer(c(song_div_q1, artist_div_q1, genre_div_q1)) %>%
  group_by(country, region_size, name) %>%
  reframe(get_boot_mean_ci(value, 'boot')) %>%
  ungroup() %>%
  mutate(name = case_match(name,
    'song_div_q1' ~ 'Song',
    'artist_div_q1' ~ 'Artist',
    'genre_div_q1' ~ 'Genre',
    'song_age' ~ 'Song Age'
  )) %>%
  mutate(name = factor(name, levels = c('Song', 'Artist', 'Genre', 'Song Age')),
         country = factor(country, levels = c('France', 'Brazil', 'Germany')))

(BID_alternatives <- pop_split_agg %>%
  ggplot(aes(region_size, boot_m, colour = country, group = country)) +
  geom_point(position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), alpha = 0.5) +
  scale_color_manual(values = c(FR_COL1, BR_COL1, DE_COL1), guide = F) +
  labs(x = 'Population size', y = 'Standardized BID') +
  facet_wrap(~ name))

plot_save(BID_alternatives, plot_name = 'main/BID_aritst_genre', size = c(70, 50))

################################################################################
## INDIVIDUAL VS. COLLECTIVE DIVERSITY
################################################################################
# relations between WID and BID for SI figure
WID_BID <- agg_stat %>%
  select(country, regional_group, metric, boot_m) %>%
  pivot_wider(names_from = metric, values_from = boot_m) %>%
  left_join(joined_pop) %>%
  group_by(country) %>%
  mutate(WID = scale(WID),
         BID = scale(BID),
         pop_size = normalize_to_one(user_pop_size_log10)) %>% 
  ungroup() %>%
  mutate(country = factor(country, levels = c('France', 'Brazil', 'Germany')))

(WID_BID_p <- WID_BID %>%
  ggplot(aes(WID, BID, colour = country)) +
  geom_point(aes(size = pop_size), alpha = 0.8) +
  add_cor() +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_size(guide = 'none') +
  scale_colour_manual(values = c(FR_COL1, BR_COL1, DE_COL1)) +
  labs(x = 'WID', y = 'BID', colour = 'Country') +
  theme(legend.position = 'top'))

plot_save(WID_BID_p, plot_name = 'SI/WID_w_BID',  size = c(150, 150))




