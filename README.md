# Population size and music diversity

This repo contains code and data for the paper "*Cultural diversity and population size in the real world"*. The data is also available for download at OSF: <https://osf.io/6zugm/>

## Codebook

`utils.R` script contains study wide packages and functions. It is called at the beginning of every script.

<br>

All aggregated data is computed from bootstrapped means and variance.

`boot_m` = bootstrap mean

`boot_me` = bootstrap median

`boot_sd` = standard deviation

`boot_lower_ci` = 2.5% quantile

`boot_upper_ci` = 97.5 quantile

<br>

Diversity is measured using Hill's number, Gini index, and GS-Score

`ind_div` = GS-Score (range 0 \~ 100)

`div_q` = defines the order *q* of Hill's number. q0 = richness, q1 = shannon entropy, q2 = simpson's index

`gini` = Gini index

------------------------------------------------------------------------

Please contact us if you run into any issues.
