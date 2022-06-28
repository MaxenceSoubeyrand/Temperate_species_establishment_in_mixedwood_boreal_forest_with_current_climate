# Validation process

We validate the model by comparing directly basal area, tree growth and plot assemblages between observations and simulations. Observations come from the Placette Echantillons Permantentes (PEP) from Quebec’s Ministry of Forests, Wildlife and Parks (MFFP). Initial conditions of simulations are the first inventory of the selected plots in PEP and we compared simulation and observation for the next inventories. To find the results of this validation process of the article, please follow this order:
- PEP_selection.R: select 10 plots in the PEP dataset in the balsam fir - yellow birch and the balsam fir - paper birch bioclimatic domain. 
- edit_param_file_validation.R: Create parameter file with initial condition the first inventory of the 10 selected plots. 
- Executeparameters files with SORTIE
- read_output_detailed.R: Transform SORTIE output in data frame. 
- combine_output_df.R: Combine the data frame create previously in one large data frame. 
- validation_PCA.R: Species assemblage validation (Fig. 4).
- validation_BA_quantile.R: Direct comparison of tree growth and plot basal area between observation and simulation. 