#' Compute within-individual genre diversity 

# load all necessary packages and functions
source('utils.R')

################################################################################
## PREPARATION
################################################################################
# load genre dictionary
genre_dict <- readRDS('data/private/genre_dict.rds')

# get raw streams
fr_stream <- readRDS('data/private/stream/fr_stream_1month_LAN.rds')
fr_stream <- fr_stream %>% left_join(genre_dict)
br_stream <- readRDS('data/private/stream/br_stream_1month_LAN.rds')
br_stream <- br_stream %>% left_join(genre_dict)
de_stream <- readRDS('data/private/stream/de_stream_1month_LAN.rds')
de_stream <- de_stream %>% left_join(genre_dict)


# get raw user data
fr_user <- readRDS('data/private/user/fr_user.rds')
br_user <- readRDS('data/private/user/br_user.rds')
de_user <- readRDS('data/private/user/de_user.rds')
joined_user <- do.call(bind_rows, list(fr_user, br_user, de_user))

# clean user data by age and outlier GS-scores
joined_user <- joined_user %>%
  select(study_user_id, country, age, WID = inv_gs_score) %>%
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

# sample 1K in each category
set.seed(42)
user_sample <- joined_user %>%
  group_by(country, age_category) %>%
  sample_n(1000)

# filter streams
fr_stream_ft <- fr_stream %>% filter(study_user_id %in% user_sample$study_user_id)
br_stream_ft <- br_stream %>% filter(study_user_id %in% user_sample$study_user_id)
de_stream_ft <- de_stream %>% filter(study_user_id %in% user_sample$study_user_id)
all_stream_ft <- do.call(bind_rows, list(fr_stream_ft, br_stream_ft, de_stream_ft))


################################################################################
## WITHIN-INDIVIDUAL GENRE DIVERSITY
################################################################################
# for each individual, grab their stream and compute the genre diversity using Hill's number order q = 1
genre_div <- all_stream_ft %>%
  group_by(study_user_id) %>%
  summarise(within_genre_div = raw_genre %>% 
           str_split(pattern = ',') %>%
           map_chr(1)  %>%
           table() %>%
           hill(q = 1))

# join back to the user sample
user_sample_update <- user_sample %>% left_join(genre_div)

# bootstrap across the participants in each group
boot_store = list()
for (i in 1:1000) {
  boot_store[[i]] <- user_sample_update %>%
    group_by(country, age_category) %>%
    sample_n(n(), replace = TRUE) %>%
    summarise(within_genre_div = mean(within_genre_div)) %>%
    cbind(boot = i)
  
  print(i)
}
boot_agg <- do.call(rbind, boot_store)

# summarise bootstrap
boot_agg <- boot_agg %>%
  group_by(country, age_category) %>%
  summarise(get_boot_mean_ci(within_genre_div, 'boot'))

# plot the trajectories
boot_agg$country <- factor(boot_agg$country, 
                              levels = c('FR', 'BR', 'DE'), 
                              labels = c('France', 'Brazil', 'Germany'))

(genre_div_p <- boot_agg %>%
    ggplot(aes(age_category, boot_m,  ymin = boot_lower_ci, ymax = boot_upper_ci, 
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
    labs(x = 'Age', y = 'Within-individual\ngenre diversity'))

# save as SI plot
genre_div_p %>% plot_save('SI/within_genre_diversity_w_age', size = c(183, 80))

