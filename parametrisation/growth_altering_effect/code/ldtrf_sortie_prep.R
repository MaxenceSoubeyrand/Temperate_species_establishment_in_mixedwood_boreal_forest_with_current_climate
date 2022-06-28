# In a first part, we modify the database of Maleki et al 2021 to combine it with 
# the RESEF database and thus have more data for the parameterization. 
# In a second part, we create parameter files to launch them in SORTIE and obtain 
# the amount of light received by each tree. The quantity of light received by each 
# tree will be useful during the parametrization since it is the variable S. 
# The parameter files thus obtained will be launched in SORTIE later. 

rm(list=ls())

#setwd à enlever après
setwd("~/PhD/Chap1/Github/parametrisation/growth_altering_effect/code")

library(tidyverse)
library(xml2)

# Preparing LDTRF data -----
# Data from Maleki et al 2021 https://doi.org/10.1002/ecy.3306
hectare_stems <- read.csv("../data/EASTERN_BOREAL_MIXEDWOODS_CANADA/hectare_stems.csv")
hectare_stem_meas <- read.csv("../data/EASTERN_BOREAL_MIXEDWOODS_CANADA/hectare_stem_meas.csv")
hectare_plots <- read.csv("../data/EASTERN_BOREAL_MIXEDWOODS_CANADA/hectare_plots.csv")
# Species names to correspond with the resef data set
sp_names <- read.csv("../data/sp_names.csv")

# Preparation of the data to LDTRF and RESEF dataset can be combined
dendro_sel <- full_join(hectare_stems, hectare_stem_meas) %>% 
  full_join(hectare_plots) %>% 
  unite(plot_id_year, c(plot_id, year), sep="_") %>% 
  filter(status_id %in% c("AB", "AS", "AL", "A")) %>% 
  filter(dbh>=10) %>% 
  mutate(Type="Adult", Height=0) %>% 
  left_join(sp_names, by=c("species_id"="ESS")) %>%
  replace_na(list(Species = "Other")) %>%
  select(x,y,Species, Type, dbh, Height, plot_id_year, lat, stem_id) %>% 
  rename(X = x, Y=y, Diam=dbh) 

# Save the prepared data
saveRDS(dendro_sel, "../results/dendro_sel_ldtrf.rds")

# Prepare parameter files for light -----
# Prepare the data to get the amount of light receive for each tree
dendro_sel <- dendro_sel %>% 
  select(-stem_id) %>% 
  nest_by(plot_id_year, lat)

# This function takes a model parameter file (calc_shade.xml) and produces parameter
# and treemap files for each PLS as well as .txt files for the treemaps
root_dir <- normalizePath(getwd())
root_dir <- str_replace(root_dir, "code", "results")
prep_sortie_files <- function(plot_id_year, lat, data) {
  write_tsv(data, paste0("../results/maps/treemap_", plot_id_year, ".txt"))
  par_xml <- read_xml("../data/calc_shade.xml")
  par_list <- as_list(par_xml)
  par_list$paramFile$plot$plot_lenX[[1]] <- 200
  par_list$paramFile$plot$plot_lenY[[1]] <- 200
  par_list$paramFile$plot$plot_latitude[[1]] <- lat
  par_list$paramFile$trees$tr_treemapFile[[1]] <- paste0(root_dir, "\\maps\\treemap_", plot_id_year, ".txt") 
  par_list$paramFile$Output$ou_filename[[1]] <- paste0(root_dir, "\\shade_res\\shade_", plot_id_year, ".gz.tar")
  par_list$paramFile$ShortOutput$so_filename[[1]] <- paste0(root_dir, "\\shade_res\\shade_", plot_id_year, ".out")
  par_xml_new <- as_xml_document(par_list)
  write_xml(par_xml_new, paste0(root_dir, "\\maps\\calc_shade_", plot_id_year, ".xml"))
}

# Create a parameter file for each plot selected and stock in the results part of parametrisation.
pwalk(dendro_sel, prep_sortie_files)
