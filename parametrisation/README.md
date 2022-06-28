# Model Parametrization

We parametrize the growth equation in two steps. We first parametrize the Diameter at Breast Height (DBH) effect, the shadow effect and the crowding effect, and we parametrize the *MaxPotGrowth* parameter as a function of climate in a second step. 

## DBH effect, shadow effect and crowding effect parametrization

In the forlder "growth_altering_effect", you can find the code and data to parametrize the Diameter at Breast Height (DBH) effect, shading effect and the crowding effect with the Réseau d’Étude et de Surveillance des Écosystèmes Forestiers RESEF dataset and data coming from Lake Duparquet Researching and Teaching Forest (LDRTF). To find the results of this parametrization process performed for this study, please follow this order:
- select_resef_plot.R: Select RESEF plots used to parametrize the model.
- ldrtf_sortie_prep.R: Prepare the LDRTF data to be used for the parametrization process and create SORTIE parameter files to find the amount of light received by each tree. 
- resef_sortie_prep.R: Prepare the RESEF data to be used for the parametrization process, create SORTIE parameter files to find the amount of light received by each tree and create a batch file to execute all the SORTIE simulation. Execute the batchfile in SORTIE to extract light received by each tree in next script. 
- sortie_extract_neighbors.R: Extract the SORTIE simulation results to extract light received by each tree and prepare data the indicate all neighbors for each trees considered with the distance of the focal tree and the neighbor tree, the DBH and the species identity of the neighbor tree. 
- nci_bayes_sp.R ; nci_bayes_sp_log.R ; nci_bayes_regroup.R ; nci_bayes_regroup_log.R: Execute these R script to compute all models (2 different error distributions and species regroup or not). These scripts use Rstan model nci_growth.Rstan (normal error distribution) and nci_growth_log.Rstan (lognormal error distribution). 
- model_selection.R: Selection of the best model among the four models for each species, and extract parameter values in tables. We also extracted information that are use to the parametrization of the *MaxPotGrowth* parametrization such as the estimated *MaxPotGrowth* for bias correction and DBH effect. 

The parametrization results are now stocked in the results folder. Next step is the *MaxPotGrowth* parametrization as a function of climate. Further details of the parametrization process in Supporting Information section B.

## *MaxPotGrowth* parametrization

In the MaxPotGrowth_parametrization.R script, we parametrize the *MaxPotGrowth* parameter using growth data coming from North America permanent plots, DBH effect found in the parametrization process of growth altering effects and climate data coming from climateNA. We used 95th quantile Bayesian model to model the tree growth as a function of climate and the DBH effect for each species. We modeled the maximum growth as the 95th quantile of the regression. We estimated *MaxPotGrowth* parameter as a function of the LDRTF climate for each species. We apply a bias correction at LDRTF site by subtracting the difference of the *MaxpotGrowth* estimate with both parametrization process in RESEF plots at LDRTF *MaxPotGrowth* estimation. Further details of the parametrization process in Supporting Information section B.