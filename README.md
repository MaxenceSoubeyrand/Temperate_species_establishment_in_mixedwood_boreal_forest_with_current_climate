# Competition and current climate do not impede temperate species to establish in boreal mixedwood forest

This repository contains the R code and data to retrieve the results and graphs from the "DOI of the article when available" study. We aimed to determine if present climate and competition are factors that prevent the establishment of temperate hardwood species, i.e., sugar maple, red maple, and yellow birch, from becoming established just above their continuous northern limit of distribution. 

## Citation 

Soubeyrand M., Gennaretti F., Blarquez O., Bergeron Y., Taylor A., D'Orangeville L. and Marchand P. (2023) Competitive interactions under current climate allow temperate tree species to grow and survive in boreal mixedwood forest. *Ecography* e06525 https://doi.org/10.1111/ecog.06525

Please cite this article for any work that re-uses this code.

## Data availability 

Initial conditions from Lake Duparquet Research and Teaching Forest (LDRTF):

- Initial conditions for unharvested stands: Maleki, K., Marchand, P., Charron, D., Lafleur, B., & Bergeron, Y. (2021). A 249‐yr chronosequence of forest plots from eight successive fires in the Eastern Canada boreal mixedwoods. Ecology, 102(5). https://doi.org/10.1002/ecy.3306

- Initial conditions for harvested stands: Brais, S., Work, T., Robert, É., O’Connor, C., Strukelj, M., Bose, A., Celentano, D., & Harvey, B. (2013). Ecosystem Responses to Partial Harvesting in Eastern Boreal Mixedwood Stands. Forests, 4(2), 364–385. https://doi.org/10.3390/f4020364

Parametrisation from Quebec’s Ministry of Forests, Wildlife and Parks (MFFP) and LDRTF:

- Spatialized data: Réseau d’Étude et de Surveillance des Écosystèmes Forestiers (RESEF) available by contacting MFFP.

- Spatialized data: Maleki, K., Marchand, P., Charron, D., Lafleur, B., & Bergeron, Y. (2021). A 249‐yr chronosequence of forest plots from eight successive fires in the Eastern Canada boreal mixedwoods. Ecology, 102(5). https://doi.org/10.1002/ecy.3306

- Growth data: DOI of article where data is presented when published or 1 year embargo.

Validation data:

- [Placettes Echantillons Permanentes](https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui)

## R packages required

- The following [tidyverse](https://www.tidyverse.org) packages: 
*dplyr*, *tidyr*, *stringr* and *ggplot*;
- the *sp*, *rgdal*, *maps*, *ggnewscale*, *ggspatial* and *raster* packages to manipulate and display spatial objects;
- the *brms*, *quantreg*, *cmdstanr* and *loo* packages to fit Bayesian model and perform model selections;
- the *xml2* package to manipulate SORTIE-ND parameter files;
- the *factoextra* and *FactoMineR* packages to compute PCA; and
- the *ggpubr*, *grid*, *ggplotify*, *gtable* and *RColorBrewer* packages working with *ggplot* package using to display results. 

## Acknowledgement

We thank the Natural Science and Engineering Research Council of Canada to support financially the project. We are grateful to Ministère des Forêts, de la Faune et des Parcs (MFFP) to provide us both RESEF and Placettes-Echantillons Permanentes datasets. This research was enabled in part by computer resources provided by [Digital Research Alliance of Canada](https://www.alliancecan.ca/en). We thank anonymous reviewers from Corrige-moi UQAT students’ group for critical revision and suggestions on the manuscript. 

## Notes 
We used the computing resources of the Digital Research Alliance of Canada to execute the Bayesian models, and the simulations with SORTIE-ND.

[![DOI](https://zenodo.org/badge/508064540.svg)](https://zenodo.org/badge/latestdoi/508064540)
