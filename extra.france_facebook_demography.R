#' Get facebook demography data and compare with our DAG user population

# load project wide packages and functions
source('utils.R')
require(psych)

# france users
fr_facebook <- read.csv('data/census/fr/fr_facebook_demographics.csv')%>% select(age_group, fb_male = male, fb_female = female)
fr_users <- readRDS('data/private/user/fr_user.rds')

# filter only users with Male or Female and age in as same category
fr_users_clean <- fr_users %>%
  filter(gender %in% c('M', 'F')) %>%
  mutate(age_group = case_when(
    age >= 13 & age < 18 ~ '13-17',
    age >= 18 & age < 25 ~ '18-24',
    age >= 25 & age < 35 ~ '25-34',
    age >= 35 & age < 45 ~ '35-44',
    age >= 45 & age < 55 ~ '45-54',
    age >= 55 & age < 65 ~ '55-64',
    age >= 65 ~ '65-older'
  )) %>%
  na.omit()

# count the numbers
fr_users_agg <- fr_users_clean %>%
  count(age_group, gender) %>%
  mutate(percent = n / sum(n) * 100) %>%
  select(-n) %>%
  mutate(gender = ifelse(gender == 'M', 'dz_male', 'dz_female'))

fr_users_wide <- fr_users_agg %>%
  pivot_wider(names_from = gender, values_from = percent)


# join the two dataframes
joined <- fr_facebook %>% left_join(fr_users_wide)

cor_result <- corr.test(joined %>% select(-age_group), method = 'spearman', adjust = 'bonferroni')
print(cor_result, short=FALSE) 
print(cor_result$stars, quote=FALSE, short=FALSE) 
