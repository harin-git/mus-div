#' This script describes the working of comparing Deezer user demographics
#' Raw user information is not available as open access.

# load project wide packages and functions
source('utils.R')

# load user data for each country
fr_users <- readRDS('data/private/user/fr_user.rds')
br_users <- readRDS('data/private/user/br_user.rds')
de_users <- readRDS('data/private/user/de_user.rds')

# join all countries and report
join_users <- do.call(bind_rows, list(br_users, fr_users, de_users))

message(sprintf(
  'Total N = %s
  France = %s,
  Brazil = %s,
  Germany = %s',
  nrow(join_users),
  nrow(fr_users),
  nrow(br_users),
  nrow(de_users)
))

################################################################################
## COMPARE USER DEMOGRAPHICS ACROSS COUNTRIES
################################################################################
clean_users <- join_users %>%
  mutate(country = factor(loc_country, levels = c('FR', 'BR', 'DE'), 
                          labels = c('France', 'Brazil', 'Germany')))

# compare age
(age <- clean_users %>%
  filter(age >= 15, age <= 80) %>%
  ggplot(aes(age, group = country, fill = country)) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_fill_manual(values = c(FR_COL1, BR_COL1, DE_COL1)) +
  labs(title = '', x = 'Age', y = 'Density'))

# compare gender
(gender <- clean_users %>%
  filter(age >= 15, age <= 80) %>%
  mutate(gender = ifelse(gender == 'M', 'Male', ifelse(gender == 'F', 'Female', 'Other'))) %>%
  filter(!is.na(gender)) %>%
  group_by(country, gender) %>%
  summarise(freq = n()) %>%
  group_by(country) %>%
  mutate(percent = freq / sum(freq)) %>%
  ggplot(aes(gender, percent, group = gender, fill = country)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  labs(title = '', x = 'Gender', y = 'Density') +
  scale_fill_manual(values = c(FR_COL1, BR_COL1, DE_COL1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none') +
  facet_wrap(~ country)
)


# compare amount of streams per each month
(monthly_stream <- clean_users %>%
  filter(n_streams <= 1000) %>%
  ggplot(aes(n_streams, group = country, fill = country)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(FR_COL1, BR_COL1, DE_COL1)) +
  labs(title = '', x = 'Numer of monthly streams\n(past 28 days)', y = 'Density'))

# export as SI figure
si_demo <- age+gender+monthly_stream+plot_layout(guides = 'collect')
si_demo %>% plot_save('SI/user_demography')


# compare within-individual diversity
(wid <- clean_users %>%
  filter(inv_gs_score > 0, inv_gs_score < 75) %>%
  ggplot(aes(inv_gs_score, group = country, fill = country)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(FR_COL1, BR_COL1, DE_COL1)) +
  labs(title = '', x = 'Inverted GS-Score', y = 'Density')
  )
wid %>% plot_save('SI/user_WID')


# compare location streaming percentage and count
(loc_percent <- clean_users %>%
    ggplot(aes(loc_play_percent, group = country, fill = country)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(FR_COL1, BR_COL1, DE_COL1)) +
    labs(title = 'Top location play', x = 'Percent locations coming\nfrom top location (%)', y = 'Density'))

(n_loc <- clean_users %>%
    ggplot(aes(n_geo_loc, group = country, fill = country)) +
    geom_density(alpha = 0.5, adjust = 10) +
    scale_fill_manual(values = c(FR_COL1, BR_COL1, DE_COL1)) +
    labs(title = 'Number of play locations', x = 'Count', y = 'Density')) +
    scale_x_continuous(limits = c(1, 50))

locs <- loc_percent + n_loc + plot_layout(guide = 'collect')
locs %>% plot_save('SI/user_location')


################################################################################
## COMPARE USER DEMOGRAPHICS WITH THE GENERAL POPULATION
################################################################################
# get nuts data for France
france_nuts_meta <- read.csv('data/census/fr/fr_nuts3_meta.csv')

fr_demo <- fr_users %>% 
  group_by(NUTS3) %>% 
  summarise(n_users = n_distinct(study_user_id),
            deez_gender = (gender[which(gender == 'F')] %>% length() / (gender[which(gender == 'M')] %>% length())),
            deez_age =  median(age, na.rm = T)
  )
compare <- france_nuts_meta %>% left_join(fr_demo) %>% ungroup()

# n users vs population
message('N users vs. population')
cor.test(compare$n_users, compare$population) %>% print()
cor.test(compare$n_users, compare$population, method = 'spearm') %>% print()

# median age
message('Median age')
cor.test(compare$deez_age, compare$population_median_age) %>% print()
cor.test(compare$deez_age, compare$population_median_age, method = 'spearm') %>% print()

# gender
message('Gender ratio')
cor.test(compare$deez_gender, compare$gender_ratio) %>% print()
cor.test(compare$deez_gender, compare$gender_ratio, method = 'spearm') %>% print()


