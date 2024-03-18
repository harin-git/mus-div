#' Draw the DAG and test for direct effect of population size on diversity
#' Raw user information is not available as open access.
#' Aggregated bootstrapped data is available at: 'data/dag'

# load all necessary packages and functions
library(dagitty)
library(ggdag)
library(propensity) # devtools::install_github("malcolmbarrett/propensity")
library(WeightIt)
library(halfmoon)
library(broom)
library(cobalt)
library(rsample)
library(ggcorrplot)
source('utils.R')


################################################################################
## PREPARATION
################################################################################
testing <- FALSE # for testing purposes

outcome <- 'WID' # 'BID' or 'WID'
n_boot <- 100 # number of bootstraps for the model. Computationally costly.


# functions for making and plotting the DAG
make_dag <- function(outcome = 'draw', exposure = 'pop_category'){
  dag_script <- 'dag {
    bb="0,0,1,1"
    age [pos="0.350,0.640"]
    BID [pos="0.200,0.570"]
    edu [pos="0.350,0.360"]
    gen [pos="0.617,0.461"]
    img [pos="0.200,0.360"]
    inc [pos="0.520,0.360"]
    WID [pos="0.520,0.570"]
    mus [pos="0.550,0.680"]
    pops [exposure, pos="0.420,0.760"]
    soc [pos="0.280,0.760"]
    age -> BID
    age -> WID
    age -> pops
    age -> soc
    edu -> BID
    edu -> WID
    edu -> soc
    gen -> inc
    gen -> WID
    img -> BID
    img -> WID
    img -> soc
    inc -> BID
    inc -> edu
    inc -> WID
    WID -> BID
    mus -> WID
    pops -> BID
    pops -> edu
    pops -> inc
    pops -> WID
    pops -> mus
    pops -> soc
    soc -> BID
    soc -> WID
    }
'
  
  # plot the base R dag
  plot_dag <- dag_script %>%
    str_replace_all('age', "Age") %>%
    str_replace_all('BID', '"BID"') %>%
    str_replace_all('edu', "Education") %>%
    str_replace_all('img', "Immigration") %>%
    str_replace_all('inc', "Income") %>%
    str_replace_all('WID', '"WID"') %>%
    str_replace_all('mus', '"Musical venues"') %>%
    str_replace_all('pops', '"Population size"') %>%
    str_replace_all('gen', "Gender") %>%
    str_replace_all('soc', '"Global friendships"') 
  
  plot_dag %>% dagitty() %>% plot()
  
  
  # add outcome
  dag_script <- switch(outcome,
                       WID = dag_script %>% str_replace('WID \\[', 'WID \\[outcome,'),
                       BID = dag_script %>% str_replace('BID \\[', 'BID \\[outcome,'),
                       draw = dag_script, # just to draw the DAG without outcome and exposure
  )
  
  dagitty(dag_script)
}

plot_dag <- function(dag, adj = NULL){
  # tidy Plot
  dag %>%
    node_status() %>%
    ggplot(aes(x, y, xend = xend, yend = yend, colour = status)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text(size = 4, colour = 'white') +
    # geom_dag_text_repel(aes(label = name), colour = 'black') +
    ggokabeito::scale_color_okabe_ito(na.value = "grey90") +
    theme_dag() +
    theme(legend.position = "none") +
    scale_y_reverse()
}

################################################################################
## DRAWING THE DAG 
################################################################################
# using the graphical interface is helpful for the positioning of nodes https://www.dagitty.net/dags.html
dag_data <- readRDS('data_output/dag_data.rds')

# make dataframe to use for the model
dag_var <- dag_data %>% 
  ungroup() %>%
  select(
    NUTS3, 
    BID = BID_nuts,
    WID = WID_nuts,
    WID_raw = WID,
    mus = log_n_music_venues,
    pops = log_pop_size_nuts,
    age,
    age_nuts,
    edu = uni_degree_percentage,
    img = log_immigrant_percentage,
    inc = log_income_per_capita,
    soc = log_global_friends,
    gen = gender,
    pop_category = pop_size_category
    ) %>%
  na.omit() %>%
  mutate(across(BID:soc, scale)) # Z-score all the continuous variables

# check if all variables are in the right format
str(dag_var)

# illustrate the DAG
dag_draw <- make_dag()

################################################################################
## DAG VARIABLE CORRELATIONS
################################################################################
if(testing){
  # check whether any correlations are perfect (i.e., collinearity)
  myCov <- cov(dag_var %>% select(BID:soc))
  round(myCov, 2)
  myCor <- cov2cor(myCov)
  noDiag <- myCor
  diag(noDiag) <- 0
  any(noDiag == 1)
  
  # if not, check for multicollinearity (i.e., is one variable a linear combination of 2+ variables?)
  det(myCov) < 0
  #or
  any(eigen(myCov)$values < 0)
  
  # correlation matrix plot (NUTS3 grouping) for SI figure
  dag_nuts_mean <- dag_var %>% 
    group_by(NUTS3, pop_category) %>%
    summarise(BID = mean(BID),
              WID = mean(WID),
              'Population size' = mean(pops),
              Age = mean(age),
              'Music venues' = mean(mus),
              Education = mean(edu),
              Immigration = mean(img),
              Income = mean(inc),
              'Social connections' = mean(soc),
              Gender = mean(as.numeric(gen))
    ) %>% 
    ungroup() 
  
  cor_var <- dag_nuts_mean %>% select(-NUTS3, -pop_category)
  corr <- round(cor(cor_var), 1)
  p.mat <- cor_pmat(cor_var)
  
  (corp <- ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE, type = "upper",
                      lab = TRUE) + theme(legend.position = 'right'))
  
  # save figure as SI
  plot_save(corp, 'SI/socio_demo_correlation_matrix', c(183, 150))
  
}

################################################################################
## DAG VARIABLE COMPARISON BY REGION SIZE
################################################################################
if(testing){
  size_region_compare <- dag_nuts_mean %>%
    mutate(across(Age:Gender, scale)) %>% # normalize the variables
    pivot_longer(Age:Gender) %>%
    mutate(pop_category = factor(pop_category, labels = c('Small', 'Medium', 'Large')))
  
  (size_region_p <- size_region_compare %>%
      ggplot(aes(pop_category, value, colour = pop_category)) +
      geom_boxplot() +
      labs(x = '', y = 'Z-score', colour = 'Population size') +
      theme(axis.text.x = element_blank(), legend.position = 'right') +
      facet_wrap(~ name, nrow = 2))
  
  # save figure as SI
  size_region_p %>% plot_save('SI/census_difference_by_region_size')
}


################################################################################
## MODEL
################################################################################
# Two models for testing
# (1) Population size on between individual diversity (BID)
# (2) Population size on within individual diversity (WID)

# draw the model DAG
dag <- make_dag(outcome = outcome)
plot_dag(dag)

## Test DAG against data
if(testing){
  # test for conditional independence
  impliedConditionalIndependencies(dag)
  dag_res <- localTests(dag, dag_var, type = 'cis', R = 10) # R defines number of bootstraps to get reliability estimates
  
  # order by effect size
  dag_res <- dag_res[order(abs(dag_res$estimate)), ]
  print(dag_res)
  
  # plot the effect size
  plotLocalTestResults(dag_res, xlim = c(-1, 1))
}

## Define minimal adjustment sets for control
adj_set_direct <- adjustmentSets(dag, effect = 'direct') %>% unlist() %>% as.character()
message(sprintf('Minimal adjustment set for direct effect: %s', paste(adj_set_direct, collapse = ', ')))

# if outcome is BID, replace the WID_nuts with raw WID where there is more granularity at user level
if(outcome == 'BID'){adj_set_direct <- str_replace(adj_set_direct, 'WID', 'WID_raw')}

## Define the models to get propensity weights and outcome model
(direct_propensity_f <- as.formula(sprintf('%s ~ %s', 'pop_category', paste(adj_set_direct, collapse = '+'))))
(model_f <- as.formula(sprintf('%s ~ %s', outcome, 'pop_category')))


################################################################################
## PROPENSITY SCORE WEIGHTING
################################################################################
if(testing){
  # Get propensity score weights
  direct_propensity_model <- weightit(
    direct_propensity_f,
    data = dag_var,
    method = "glm",
    estimand = "ATO"
  )
  
  # add the weights to the data frame
  dag_var_weighted <- dag_var %>% cbind(weights = direct_propensity_model$weights)
  
  # diagnostics of the propensity weights
  summary(direct_propensity_model)
  cobalt::bal.tab(direct_propensity_model, stats = 'mean.diffs', disp = c("means", "sds"), weights = "weights")
  direct_propensity_model$weights %>% density() %>% plot()
  
  v <- data.frame(old = c("age", "edu", "gen_M", "img", 'inc', 'mus', 'soc', 'WID'),
                  new = c("Age", "Education", "Gender", 
                          "Immigration", "Income", "Music venues", "Social connection", 'WID'))
  
  (lovep <- love.plot(direct_propensity_model, stats = c("mean.diffs"), 
                      threshold = c(m = .1), 
                      binary = "std",
                      abs = TRUE,
                      var.order = "unadjusted",
                      var.names = v,
                      # limits = c(0, 1),
                      grid = FALSE,
                      wrap = 20,
                      sample.names = c("Unadjusted", "Adjusted"),
                      position = "top",
                      shapes = c("circle", "triangle"),
                      colors = c("red", "blue")) +
      labs(title = sprintf('%s model covariate balance', outcome)))
  
  # save the love plot as SI figure
  lovep %>% plot_save(sprintf('SI/%s_love_plot', outcome), c(92, 80))
  
  
  # compare the weighted matches before and after weighting
  before_after <- dag_var_weighted %>% 
    cbind(type = 'Weighted') %>% 
    rbind(dag_var_weighted %>% mutate(weights = 1) %>% cbind(type = 'Unweighted'))
  
  # compare direct effect
  (weight_compare_p <- before_after %>%
      mutate(pop_category = factor(pop_category, labels = c('small', 'mid', 'large'))) %>%
      ggplot(aes(x = WID, color = !!sym(exposure))) +
      geom_ecdf(aes(weights = weights)) +
      labs(subtitle = sprintf('%s model ECDF', outcome), 
           x = outcome, 
           y = 'Proportion <= x',
           color = 'Population size\nquantile') +
      facet_wrap(~ type))
  
  # save the ECDF graph for weighted and unweighted comparison as SI figure
  weight_compare_p %>% plot_save(sprintf('SI/%s_weight_ecdf', outcome), c(92,80))
  
  # quick test of the model
  lm(model_f, data = dag_var_weighted) %>% summary()
  lm(model_f, data = dag_var_weighted, weights = dag_var_weighted$weight) %>% summary() 
}

################################################################################
##  BOOTSTRAP MODEL ESTIMAND
################################################################################
bootstrap_model <- function(n_boot){
  message('Starting bootstrap...')
  storage = list()
  for (i in 1:n_boot) {
    # resample the data with replacement
    resampled_dt <- dag_var %>%
      group_by(pop_category) %>%
      sample_n(n(), replace = TRUE)
    
    # get propensity score
    message('Calculating propensity weights...')
    propensity <<- weightit(
      direct_propensity_f,
      data = resampled_dt,
      method = "glm",
      estimand = "ATO"
    )
    
    # test the model
    unadj_model <- lm(model_f, data = resampled_dt) # no weights
    unadj_model <- cbind(tibble(type = 'Unadjusted', adj_r_square = summary(unadj_model)$adj.r.squared), unadj_model %>% tidy())
    
    adj_model <- lm(model_f, data = resampled_dt, weights = propensity$weights)
    adj_model <- cbind(tibble(type = 'Adjusted', adj_r_square = summary(adj_model)$adj.r.squared), adj_model %>% tidy())
    
    join_model <- cbind(boot = i, rbind(unadj_model, adj_model))

    # prepare the outcome
    outcome <- join_model %>%
      mutate(
        category = case_match(
          term,
          '(Intercept)' ~ 'small',
          sprintf('%s2', 'pop_category') ~ 'medium',
          sprintf('%s3', 'pop_category') ~ 'large',
        ) %>% factor(levels = c('small', 'medium', 'large'))
      )
  
    # store 
    storage[[i]] <- outcome
    print(sprintf('Done with boot %s', i))
  }
  do.call(rbind, storage)
}

# bootstrap
boot_results <- bootstrap_model(n_boot)
boot_results %>% write_rds(sprintf('bootstrap_output/%s_DAG_model_B%s_boot.rds', outcome, n_boot))

