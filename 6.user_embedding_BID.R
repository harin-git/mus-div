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

# plot UMAP and perform KDE over all regions
umap_df %>%
    ggplot(aes(x, y)) +
    stat_density_2d(aes(fill = ..density..), 
                    geom = "raster", 
                    contour = FALSE, 
                    h = 3,
                    n = 50) +
    labs(y = 'Dim.2', x = 'Dim.1') +
    facet_wrap(~ NUTS3)

################################################################################
## REGIONAL EXAMPLES
################################################################################
# plot example regions
paris <- 'FR101'
orne <- 'FRD13'

make_example_regional <- function(city, h = 2, max_lim = 0.05){
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
    scale_y_continuous(limits = c(-6, 6)) +
    scale_x_continuous(limits = c(-10, 10))
}

(paris_raster <- make_example_regional(paris, h = 3, max_lim = 0.04))
(orne_raster <- make_example_regional(orne, h = 3, max_lim = 0.04))

(joined_raster <- paris_raster + orne_raster + plot_layout(guide = 'collect') & theme(legend.position = 'right'))
joined_raster %>% plot_save('main/umap_user_cluster', c(113, 50))


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
## CORRELATION WITH HILL NUMBER BID
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
  cos_sd <- embed %>% as.matrix() %>% lsa::cosine() %>% sd()
  
  storage[[i]] <- tibble(NUTS3 = regions[i], cos_m, cos_sd)
}
cosine_output <- do.call(rbind, storage)

# bind with hill's number diversity data
fr_bid <- read_rds('data/diversity/FR_diversity.csv') %>% 
  filter(metric == 'song_div_q1') %>%
  select(NUTS3 = regional_group, BID = boot_m, user_pop_size_log10)

# STATS: get correlations with frequency based BID and with population size
corr <- cosine_output %>% left_join(fr_bid)

# by mean cosine similarity
cor.test(corr$cos_m, corr$BID, method = 'pearson', alternative = "two.sided")
cor.test(corr$cos_m, corr$user_pop_size_log10, method = 'pearson', alternative = "two.sided")

# by sd cosine similarity
cor.test(corr$cos_sd*-1, corr$BID, method = 'pearson', alternative = "two.sided")
cor.test(corr$cos_sd*-1, corr$user_pop_size_log10, method = 'pearson', alternative = "two.sided")
