# Mus-Div

This repo contains code and data for the paper "*Why cities vary in their cultural diversity"*. The data is also available for download at OSF: <https://osf.io/6zugm/>

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
One with BID computed that does not exclude algorithmic recommendations (labelled 'combined'), while the other separates algorithmic recommended streams with organic streams. This is indicated as: 

`listen_type` = 'O' corresponding to 'organic' and 'A' corresponding to 'algorithmic'

## Data Sources
The causal estimation using DAG in our paper focuses specifically on French users. We selected France for our analysis due to its extensive, publicly available demographic data. To strengthen our analysis, we incorporated additional data sources, including Facebook friendship connections and regional music venue statistics. All of these data are compiled and stored in the `data/census/fr` directory.

Below lists the original sources of the data we gathered:

`Population size` = Eurostat census data available at https://ec.europa.eu/eurostat/web/main/data/database

`Age, Gender, Immigration, Education, Income` = compiled longitudinal census data by Cagé and Piketty (2023), available at https://unehistoireduconflitpolitique.fr/telecharger.html. The raw data originates from INSEE (https://www.insee.fr).

`Social connectedness index` = region pair Facebook friendship connections released by Meta, available at https://data.humdata.org/dataset/social-connectedness-index

`Music venues` = list of music venues by location can be queried using the Songkick's API https://www.songkick.com/developer by requesting access as a developer. Each venue search results comes with rich meta data including their geographical coordinates.

------------------------------------------------------------------------

Please contact the lead author Harin Lee (www.harinlee.info) if you run into any issues.