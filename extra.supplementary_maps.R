#' Supplementary maps for regional socio-demographics across France

# load all necessary packages and functions
source('utils.R')

# load French map polygons
fr_map <- readRDS('data/census/fr/fr_map.rds') 

## Music venues
songkick <- read.csv('data/census/fr/fr_music_venues.csv')
songkick_coord <- songkick %>%
  filter(loc_long > -30, loc_lat > 40, !is.na(loc_long)) %>%
  sf::st_as_sf(coords = c("loc_long", "loc_lat"), crs = 4326)

(songkick_map <- ggplot() +
  geom_sf(data = fr_map, fill = FR_COL1, color="gray", size=.01, show.legend = FALSE) +
  geom_sf(data = songkick_coord, size = 0.05, alpha = 0.5, colour = 'gray') +
  labs(title = 'Musical venues', 
       subtitle = sprintf('N = %s', nrow(songkick_coord))
       )
)

songkick_map %>% plot_save('SI/songkick_map', c(90, 90))


## CENSUS (INSEE)
fr_commune <- readRDS('data/census/fr/fr_gisco_muni_2020.rds')
fr_census <- readRDS('data/census/fr/fr_insee_2022_meta.rds')

census_join <- fr_commune %>% 
  filter(muni_lng > -30, muni_lat > 40, !is.na(muni_lng)) %>% 
  left_join(fr_census %>% select(muni_code = insee_code, income_per_capita, uni_degree_percentage, immigrant_percentage))

census_join <- census_join %>%
  mutate(across(uni_degree_percentage:immigrant_percentage, function(x)x*100)) %>%
  mutate(income_per_capita = income_per_capita / 1000)


# define plotting function to map each attribute
make_map_plot <- function(df, variable, n_breaks = 8, 
                          plot_title = '', 
                          scale_name = '', col_palette = 'Lajolla'){
  df <- df %>%
    mutate(values_cut = ntile(!!sym(variable), n_breaks -1) %>% as.factor())
  
  # make break points using log scale
  br <- df %>%
    select(values_cut, !!sym(variable)) %>%
    as_tibble() %>%
    group_by(values_cut) %>%
    summarise(cut = max(!!sym(variable)))
  br <- br$cut
  
  # label
  labs_plot <- prettyNum(br %>% signif(2), big.mark = ",")
  
  # palette
  pal <- hcl.colors(length(br) - 1, col_palette, rev = TRUE)
  
  # plot
  df %>%
    ggplot(aes(fill = values_cut, colour = values_cut)) +
    geom_sf() +
    labs(title = plot_title) +
    scale_fill_manual(
      name = scale_name,
      values = pal,
      labels = labs_plot,
      drop = FALSE,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = 0.5,
        keywidth = 2.5,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = .5,
        nrow = 1,
        byrow = TRUE,
        reverse = FALSE,
        label.position = "bottom"
      )
    ) +
    scale_colour_manual(
      name = scale_name,
      values = pal,
      labels = labs_plot,
      drop = FALSE,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = 0.5,
        keywidth = 2.5,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = .5,
        nrow = 1,
        byrow = TRUE,
        reverse = FALSE,
        label.position = "bottom"
      )
    )
  
}

income <- make_map_plot(census_join, 
              'income_per_capita', 
              plot_title = 'Income',
              scale_name = 'Average per capita income (1K)')

education <- make_map_plot(census_join, 
                        'uni_degree_percentage', 
                        plot_title = 'Education',
                        scale_name = "Population with Bachelor's degree (%)")

immigration <- make_map_plot(census_join, 
                           'immigrant_percentage', 
                           plot_title = 'Immigration',
                           scale_name = "Immigrant population (%)")

(joined_census_plot <- income + education + immigration)
joined_census_plot %>% plot_save('SI/france_census_map', c(350, 150))

