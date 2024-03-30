#' This script includes all functions custom functions for the analyses.
#' It is loaded in every analysis script.

# load all required package to reproduce the analyses
library(tidyverse) # general tidy framework
library(vegan) # for diversity measures
library(patchwork) # for combining plots
library(lubridate) # for time formatting
library(sf) # for transformation of geo coordinates
library(rnaturalearth) # for polygons of regional boundaries
library(DescTools) # for Gini-coefficient
library(gam) # for GAM model fitting
library(scales) # for adding log scales to axis

################################################################################
## THEME
################################################################################
# flag colours of countries
FR_COL1 = '#002654'
FR_COL2 = '#ED2939'
FR_COL3 = '#FFFFFF'

BR_COL1 = '#009739'
BR_COL2 = '#FEDD00'
BR_COL3 = '#012169'

DE_COL1 = '#FFCC00'
DE_COL2 = '#000000'
DE_COL3 = '#DD0000'


# Themes setup
options(
  # set default colors in ggplot2 to colorblind-friendly 
  # Okabe-Ito and Viridis palettes
  ggplot2.discrete.colour = ggokabeito::palette_okabe_ito(),
  ggplot2.discrete.fill = ggokabeito::palette_okabe_ito(),
  ggplot2.continuous.colour = 'viridis',
  ggplot2.continuous.fill =  'viridis',
  # set theme font and size
  book.base_family = "sans",
  book.base_size = 10
)

# set default theme
theme_set(
  theme_minimal(
    base_size = getOption("book.base_size"), 
    base_family = getOption("book.base_family")
  ) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    )
)

# DAG theme
theme_dag <- function() {
  ggdag::theme_dag(base_family = getOption("book.base_family"))
}


################################################################################
## DATA LOADER
################################################################################
DATA_PATH <- 'PATH_TO_RAW_FILES'

load_user_data <- function(country = c('fr', 'br')){
  switch(country,
         fr = {
           readRDS(file.path(DATA_PATH, 'fr_user.rds'))
         },
         br = {
           readRDS(file.path(DATA_PATH, 'br_user.rds'))
         }
  )}
load_stream_data <- function(country = c('fr', 'br')){
  switch(country,
         fr = {
           readRDS(file.path(DATA_PATH, 'fr_stream_1month.rds'))
         },
         br = {
           readRDS(file.path(DATA_PATH, 'br_stream_1month.rds'))
         }
  )}
load_analysis_data <- function(country = c('fr', 'br')){
  switch(country,
         fr = {
           readRDS(file.path(DATA_PATH, 'fr_user.rds'))
         },
         br = {
           readRDS(file.path(DATA_PATH, 'br_user.rds'))
         }
  )}

load_region_meta_data <- function(){readRDS(file.path('data', 'nuts3_socio_demo.rds'))}

# load all data frome a directory in batch
load_batch <- function(dir_path, drop_loc_cols = F){
  all_files <- list.files(dir_path)
  all_files <- all_files[str_detect(all_files, '.csv')]
  file_paths <- file.path(dir_path, all_files)
  
  if(drop_loc_cols){
    read_drop <- function(x){
      read.csv(x) %>% select(- loc_lat, - loc_long, - loc_city)
    }
      
    load_files <- purrr::map(file_paths, read_drop)
    
  } else {
    load_files <- purrr::map(file_paths, read.csv)
  }
  
  return(do.call(rbind, load_files))
}

################################################################################
## STATS
################################################################################
# explore density distribution of the data
test_norm <- function(x, title, method = 'dens'){
  par(mfrow = c(1, 2))
  
  switch (method,
    dens = density(x, na.rm = T) %>% plot(main = title)
  )

  qqnorm(x)
}

# calculate the margin of error
margin <- function(x, alpha = 0.05){
  mean_value <- mean(x, na.rm = T)
  
  # Compute the size
  n <- length(x)
  
  # Find the standard deviation
  standard_deviation <- sd(x, na.rm = T)
  
  # Find the standard error
  standard_error <- standard_deviation / sqrt(n)
  degrees_of_freedom = n - 1
  t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
  margin_error <- t_score * standard_error
  return(margin_error)
}

# calculate mean and CI
get_boot_mean_ci <- function(x, var_name, alpha = 0.05, ...){
  low <- alpha / 2
  high <- 1 - alpha / 2
  output <- tibble(m = mean(x),
                   med = median(x),
                   sd = sd(x),
                   lower_ci = quantile(x, low),
                   upper_ci = quantile(x, high)
  )
  colnames(output) <- paste(var_name, colnames(output), sep = '_')
  return(output)
}

# fit GAM function to bootstapped data
gam_fit <- function(x, y){
  boot_data <- data.frame(x = x, y = y)
  output_data <- data.frame(x = seq(boot_data$x %>% min(), boot_data$x %>% max(), length.out = 1000))
  gam_model <- gam::gam(y ~ s(x, 2), data = boot_data)
  # plot(gm)
  output_data$y <- predict(gam_model, output_data, type = 'response')
  output_data
}

# normalize numbers from 0 to 1
normalize_to_one <- function(x, ...) {
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}


# get indendent t stats of the mean, CI and p
get_t_independent <- function(var){
  t_stat <- t.test(var)
  p <- t_stat$p.value[1]
  
  t <- t_stat$statistic[1]
  
  m <- t_stat$estimate[1]
  lower_ci <- t_stat$conf.int[[1]]
  upper_ci <- t_stat$conf.int[[2]]
  
  tibble(t, m, lower_ci, upper_ci, p)
  
}

# get t-statistics and effect size as Cohen's D
get_t_stat <- function(var1, var2){
  t_stat <- t.test(var1, var2)
  n1 <- length(var1)
  n2 <- length(var2)
  
  p <- t_stat$p.value
  t <- t_stat$statistic
  
  lower_ci <- t_stat$conf.int[[1]] * -1
  upper_ci <- t_stat$conf.int[[2]] * -1
  
  mean_diff <- t_stat$estimate %>% diff()
  pooled_sd <- sqrt(((n1 - 1) * sd(var1)^2 + (n2 - 1) * sd(var2)^2) / (n1 + n2 - 2))
  cohensD <- abs(mean_diff) / pooled_sd
  
  tibble(t, p, mean_diff, lower_ci, upper_ci, pooled_sd, cohensD)
}

# calculate hills number
hill <- function(abundance_vector, q){
  if(q == 0){
    return(length(abundance_vector))
  } else if(q == 1){
    p <- abundance_vector / sum(abundance_vector)
    return(exp(-sum(p * log(p))))
  } else if(q == 2){
    p <- abundance_vector / sum(abundance_vector)
    return(1 / sum(p^2))
  } else {
    stop("Unsupported Hill number. Only 0, 1, and 2 are supported.")
  }
}

# grab a random one from a single string with multiple elements
grab_random <- function(vec, separator = ',', n_sample = 1){
  if(!is.na(vec)){
    vec %>% 
      str_split(pattern = separator) %>% 
      unlist() %>% 
      sample(n_sample)
  } else {
    return(NA)
  }
}

# add correlation to ggplot
add_cor <- function(method = 'pearson') {
  ggpubr::stat_cor(
    method = method,
    p.accuracy = 0.01,
    r.accuracy = 0.01 )
}


################################################################################
## MAPS & USER MAPPING
################################################################################
# map latitude and longitude from the stream data to NUTS3 region
get_nuts <- function(df){
  df <- df %>% mutate(index  = 1:nrow(df))
  
  nuts_out <- df %>%
    select(index, lat, lng) %>%
    filter(!is.na(lat) | !is.na(lng)) %>%
    sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
    sf::st_join(giscoR::gisco_nuts) %>%
    subset(LEVL_CODE == 3) %>%
    as_tibble() %>%
    select(index, NUTS_name = NUTS_NAME, NUTS3 = NUTS_ID)
  
  df %>% left_join(nuts_out) %>% select(- index)
}

# french map
plot_france_map <- function(){
  fr_map <- readRDS('data/fr_map.rds')
  
  country_lines <- fr_map %>%
    group_by(
      CNTR_CODE
    ) %>%
    summarise(n = n()) %>%
    st_cast("MULTILINESTRING") %>%
    filter(CNTR_CODE == 'FR')
  
  fr_map %>%
    ggplot() +
    geom_sf(linewidth = 0.5, color = 'black', alpha = 1) + 
    geom_sf(data = country_lines, col = "black", linewidth = 0.1) +
    # Center in France
    coord_sf(
      xlim = c(3277294, 4253440),
      ylim = c(2013597, 3228510)) 
}


# get lat and lng from geom by taking the centroid
get_lat_lng_from_geom <- function(geom_point) {
  pnts <- geom_point %>% unlist() %>% as.numeric()
  lng <- pnts[1]
  lat <- pnts[2]
  tibble(lat, lng)
}


# using the latitude and longitude of the user location, map to the geom boundaries
map_users <- function(user_data, geom_data, CRS = 4326){
  message('Converting user location to SF coordinates')
  user_points <- user_data %>%
    select(user_id, loc_long, loc_lat) %>%
    sf::st_as_sf(coords = c("loc_long", "loc_lat"), crs = CRS)
  
  message('Mapping user to region')
  mapping <- sf::st_within(user_points, geom_data) %>% as.character() %>% as.numeric()
  
  message('Binding the data')
  mapped_users <- geom_data[mapping, ] %>% as_tibble() %>% select(muni_code)
  
  unmapped_users <- is.na(mapped_users$muni_code) %>% sum()
  message(sprintf('There were %s unmatched users (%s percent)', unmapped_users, round(unmapped_users/nrow(user_data)*100, 1)))
  
  # keep only the mapped ones
  user_data %>%
    cbind(mapped_users) %>%
    filter(!is.na(muni_code))
}


################################################################################
## PLOT
################################################################################
# save plot
plot_save <- function(plot = last_plot(), plot_name, size = c(183, 100), pdf = TRUE){ 
  # default size for journal is 183mm wide
  
  # saves both pdf and png versions of the plot
  if(pdf){ # when too much data pdf becomes too large
    ggsave(
      filename = paste0(plot_name, '.pdf'),
      plot = plot,
      width = size[1],
      height = size[2],
      units = 'mm',
      path = 'figure_output',
      dpi = 'print',
      device = 'pdf'
    )
  }

  ggsave(
    filename = paste0(plot_name, '.png'),
    plot = plot,
    width = size[1],
    height = size[2],
    units = 'mm',
    path = 'figure_output',
    dpi = 'print',
    device = 'png'
  )
}

# drawing the DAG
geom_dag_label_repel <- function(..., seed = 10) {
  ggdag::geom_dag_label_repel(
    aes(x, y, label = label),
    box.padding = 3.5, 
    inherit.aes = FALSE,
    max.overlaps = Inf, 
    family = getOption("book.base_family"),
    seed = seed,
    label.size = NA, 
    label.padding = 0.1,
    size = getOption("book.base_size") / 3,
    ...
  ) 
}


