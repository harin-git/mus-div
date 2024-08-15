# Music cultural diversity

This repo contains code and data for the paper "*Mechanisms of cultural diversity through 2.5 million individuals’ music listening patterns"*. The data is also available for download at OSF: <https://osf.io/6zugm/>

## Codebook

`utils.R` script contains study wide packages and functions. It is called at the beginning of every script. If you are missing any of the packages, they need to be download first.

<br>

All aggregated data is computed from bootstrapped means and variance.

`boot_m` = bootstrap mean

`boot_me` = bootstrap median

`boot_sd` = standard deviation

`boot_lower_ci` = 2.5% quantile

`boot_upper_ci` = 97.5 quantile

<br>

Diversity is measured using Hill's number, Gini index, and GS-Score.

`ind_div` = GS-Score (range 0 \~ 100)

`div_q` = defines the order *q* of Hill's number. q0 = richness, q1 = shannon entropy, q2 = simpson's index

`gini` = Gini index

<br>
Diversity measure outcomes are available in the folder `data/diversity` and comes in two versions.
One with BID computed that does not exclude algorithmic recommendations (labelled 'combined'), while the other separates algorithmic recommended streams with organic streams. This is indicated as: `listen_type` = 'O' corresponding to 'organic' and 'A' corresponding to 'algorithmic'

------------------------------------------------------------------------

Please contact us if you run into any issues.
