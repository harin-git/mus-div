#' Use user embedding to calculate the dispersion of users within regions.
#' It is an alternative metric to frequency-based BID measure that considers relations between songs and users.
#' Raw user information is not available as open access.

# load project wide packages and functions
source('utils.R')

################################################################################
## PREPARATION
################################################################################
fr_users <- read_rds('data/private/user/fr_user.rds')
fr_embedding <- read_rds('data/private/fr_embedding.rds')
fr_pop_size <- read.csv('data/diversity/FR_diversity.csv') %>% 
  select(NUTS3 = regional_group, n_users) %>%
  distinct(NUTS3, n_users)

fr_joined <- fr_embedding %>% na.omit() %>% left_join(fr_users %>% select(study_user_id, NUTS3, NUTS3_name))

# users are sampled without replacement to obtain unique samples
set.seed(42)
sampled_users <- fr_joined %>%
  filter(!is.na(NUTS3)) %>%
  group_by(NUTS3) %>%
  sample_n(500, replace = F) %>% # initially over sample a balanced set
  ungroup()

# drop access to have 10000 samples
sampled_users <- sampled_users %>% sample_n(10000, replace = F)

# unpack user embedding
feat <- reshape::colsplit(sampled_users$user_embedding, ',', names = paste0('dim', 1:128))
meta <- sampled_users %>% select(study_user_id, NUTS3, NUTS3_name) 

################################################################################
## UMAP
################################################################################
# make UMAP embedding
set.seed(42)
umap_feat <- feat %>% uwot::umap(n_neighbors = 5, 
                                 n_components = 2,
                                 metric = 'cosine',
                                 scale = T)
umap_df <- cbind.data.frame(setNames(as.data.frame(umap_feat), c("x", "y")), meta)

# save the umap output
umap_df %>% write_rds('data_output/user_embedding_umap.rds')

# add population size meta data
umap_df <- umap_df %>% left_join(fr_pop_size)

# plot UMAP and perform KDE over all regions
(umap_all <- umap_df %>%
    ggplot(aes(x, y)) +
    stat_density_2d(aes(fill = ..density..), 
                    geom = "raster", 
                    contour = FALSE, 
                    h = 4,
                    n = 100) +
    labs(y = 'Dim.2', x = 'Dim.1') +
    facet_wrap(~ reorder(NUTS3_name, -n_users), ncol = 10))

umap_all %>% plot_save('SI/fr_bid_umap_all', c(250, 300))

################################################################################
## REGIONAL EXAMPLES
################################################################################
# plot example regions
paris <- 'FR101'
orne <- 'FRD13'

make_example_regional <- function(city, h = 2, max_lim = 0.05, SI = FALSE){
  ggplot() +
    geom_point(
      data = umap_df,
      aes(x, y),
      size = 0.0001,
      alpha = 0.03) +
    stat_density_2d(
      data = umap_df %>% filter(NUTS3 == city),
      aes(x, y, fill = ..density..),
      geom = "raster",
      contour = FALSE,
      h = h,
      alpha = 0.8,
      n = 512
    ) +
    scale_fill_gradient2(
      low = 'white',
      high = FR_COL1,
      midpoint = 0.006,
      limits = c(0, max_lim),
      breaks = c(0, max_lim)
    ) +
    labs(
      y = 'Dim. 2',
      x = 'Dim. 1',
      title = '',
      caption = "",
      fill = 'Density'
    ) +
    (if(SI){theme_void() + theme(legend.position = 'none')}) +
    scale_y_continuous(limits = c(-6, 6)) +
    scale_x_continuous(limits = c(-10, 10))
}

(paris_raster <- make_example_regional(paris, h = 3, max_lim = 0.04))
(orne_raster <- make_example_regional(orne, h = 3, max_lim = 0.04))

(joined_raster <- paris_raster + orne_raster + plot_layout(guide = 'collect') & theme(legend.position = 'right'))
joined_raster %>% plot_save('main/umap_user_cluster', c(113, 50))

# add several more to SI
make_example_regional('FR107', h = 3, max_lim = 0.04, SI = T) %>%
  plot_save('SI/val-de-marne', c(30, 30)) # val-de-marne
make_example_regional('FRL05', h = 3, max_lim = 0.04, SI = T) %>%
  plot_save('SI/var', c(30, 30)) # var
make_example_regional('FRC12', h = 3, max_lim = 0.04, SI = T) %>%
  plot_save('SI/nievre', c(30, 30)) # nievre
make_example_regional('FRG01', h = 3, max_lim = 0.04, SI = T) %>%
  plot_save('SI/loire-atlantique', c(30, 30)) # loire-atlantique
make_example_regional('FRM01', h = 3, max_lim = 0.04, SI = T) %>%
  plot_save('SI/corse-du-sud', c(30, 30)) # corse-du-sud


# make small maps for inset into the figures
fr_map <- readRDS('data/census/fr/fr_map.rds')

country_lines <- fr_map %>%
  group_by(
    CNTR_CODE
  ) %>%
  summarise(n = n()) %>%
  sf::st_cast("MULTILINESTRING") %>%
  filter(CNTR_CODE == 'FR')


(france_map <- fr_map %>%
    ggplot() +
    geom_sf(linewidth = 0, color = 'black', alpha = 1) + 
    geom_sf(data = country_lines, col = "gray50", linewidth = 1) +
    # Center in France
    coord_sf(
      xlim = c(3277294, 4253440),
      ylim = c(2013597, 3228510)) +
    theme_void())
france_map %>% plot_save('main/france_map', size = c(30,30))



################################################################################
## CORRELATION WITH HILL'S NUMBER BID
################################################################################
# sample a equal number of 800 users (smallest region reference) as doing it on the entire set is too costly
set.seed(42)
user_subset <- fr_joined %>%
  group_by(NUTS3) %>%
  sample_n(800, replace = F)

# unpack the embeddings
user_subset <- reshape::colsplit(user_subset$user_embedding, ',', names = paste0('dim', 1:128)) %>%
  cbind(NUTS3 = user_subset$NUTS3)

# get cosine similarity across all pairs of users within the region
regions <- user_subset$NUTS3 %>% unique()
storage = list()
for (i in 1:length(regions)) {
  embed <- user_subset %>%
    filter(NUTS3 == regions[i]) %>%
    select(starts_with('dim'))
  
  cos_m <- embed %>% as.matrix() %>% lsa::cosine() %>% mean()
  cos_m <- 1 - cos_m
  cos_sd <- embed %>% as.matrix() %>% lsa::cosine() %>% sd()
  cos_sd <- 1 - cos_sd
  
  storage[[i]] <- tibble(NUTS3 = regions[i], cos_m, cos_sd)
}
cosine_output <- do.call(rbind, storage)

# bind with hill's number diversity data
fr_bid <- read_csv('data/diversity/FR_diversity.csv') %>% 
  filter(listen_type == 'O', metric == 'song_div_q1') %>%
  select(NUTS3 = regional_group, BID = boot_m, user_pop_size_log10)

# STATS: get correlations with frequency based BID and with population size
corr <- cosine_output %>% left_join(fr_bid)
correlation::correlation(corr %>% select(cos_sd, BID, user_pop_size_log10))

# add region labels for plotting
fr_regions <- read.csv('data/census/region_codebook.csv') %>%
  filter(country_code == 'FR') %>% select(NUTS3 = regional_group, region_name = state_name) %>%
  distinct()
corr <- corr %>% left_join(fr_regions)

# make plots for main
(cosine_plot_main <- corr %>%
    ggplot(aes(cos_sd, BID)) +
    geom_point(size = 0.7, alpha = 0.5, colour = FR_COL1) +
    geom_smooth(method = 'lm', se = F, colour = FR_COL1) +
    # add_cor() +
    labs(x = 'User dispersion', y = 'BID'))

plot_save(cosine_plot_main, 'main/bid_user_dispersion', c(50, 40))

# make plots for SI
(cosine_plot_si <- corr %>%
    ggplot(aes(cos_sd, BID)) +
    geom_point(alpha = 0.2, colour = 'blue') +
    geom_smooth(method = 'lm', se = F) +
    add_cor() +
    labs(x = 'User dispersion', y = 'BID') +
    geom_text(aes(label = region_name), nudge_y = 0.01, check_overlap = T, size = 2.5))

plot_save(cosine_plot_si, 'SI/bid_user_dispersion', c(183, 150))



