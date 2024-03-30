#' Plot bootstrap DAG model outcomes

# load all necessary packages and functions
source('utils.R')

WID_model <- readRDS('bootstrap_output/WID_DAG_model_B100_boot.rds') %>% cbind(dag = 'WID')
BID_model <- readRDS('bootstrap_output/BID_DAG_model_B100_boot.rds') %>% cbind(dag = 'BID')

joined_models <- rbind(WID_model, BID_model)


# aggregate the bootstraps
model_agg <- joined_models %>%
  # mutate(estimand = ifelse(category != 'small', estimate, estimate[1] + estimate)) %>%
  mutate(estimand = ifelse(category != 'small', estimate, 0)) %>%
  group_by(dag, type, category) %>%
  reframe(get_boot_mean_ci(estimand, 'boot')) %>%
  mutate(type = factor(type, levels = c('Adjusted', 'Unadjusted')))
model_agg %>% write_csv('data/dag/dag_outcome.csv')

# STATS: report in text
model_agg

# plot for BID and WID
(bid_p <- model_agg %>%
    filter(dag == 'BID') %>%
    ggplot(aes(category, boot_m, ymin = boot_lower_ci, ymax = boot_upper_ci, colour = type, group = type)) +
    geom_point() +
    geom_errorbar(size = 0.2, width = 0.5) +
    geom_line(aes(linetype = type)) +
    labs(x = 'Population size', y = 'Standardised estimate') +
    theme(legend.position = 'none') +
    # ylim(limits = c(-1.5, 2)) +
    scale_colour_manual(values = c('#C41E3A', "gray80")))
bid_p %>% plot_save('main/BID_dag', c(60, 50))

(wid_p <- model_agg %>%
    filter(dag == 'WID') %>%
    ggplot(aes(category, boot_m, ymin = boot_lower_ci, ymax = boot_upper_ci, colour = type, group = type)) +
    geom_point() +
    geom_errorbar(size = 0.2, width = 0.5) +
    geom_line(aes(linetype = type)) +
    labs(x = 'Population size', y = 'Standardised estimate') +
    theme(legend.position = 'none') +
    scale_colour_manual(values = c('#C41E3A', "gray80")))
wid_p %>% plot_save('main/WID_dag', c(60, 50))



