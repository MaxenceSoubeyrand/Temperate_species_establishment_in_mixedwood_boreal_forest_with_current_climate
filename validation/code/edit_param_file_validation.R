# We load the data of the selected permanent plots, the growth data according to 
# the climate and create parameter files ready to be executed on SORTIE. 

rm(list=ls())

library(xml2)
library(tidyverse)

# Load functions that modify parameter files
source("edit_param_functions.R")

# Growth table from MaxPotGrowth parametrisation models 
LDTRF_PEP_growth <- readRDS("../../parametrisation/MaxPotGrowth/results/LDTRF_PEP_MaxPotGrowth.rds")
PEP_growth <- LDTRF_PEP_growth %>% 
  filter(!ID %in% c("FERLD")) %>% 
  droplevels() %>% 
  select(ID, species, growth_no_bias) %>% 
  mutate(species=str_replace(species,"\n", "_"))

# Initial conditions for validation data -> PEP data
data_val <- readRDS("../data/PEP_validation_classe.rds") 

# Changes the species name in the validation data to match the function 
data_val$species_id[data_val$species_id=="THO"] <- "TOC"
data_val$species_id[data_val$species_id=="SAB"] <- "ABA"
data_val$species_id[data_val$species_id=="EPB"] <- "PGL"
data_val$species_id[data_val$species_id=="PIG"] <- "PBA"
data_val$species_id[data_val$species_id=="PET"] <- "PTR"
data_val$species_id[data_val$species_id=="BOP"] <- "BPA"


out_dir <- "/path/where/output/will/be/stocked" # Where the output will be directed.
par_dir <- "../data/parameter_files" # Where the parameter files will be directed

# Create as many parameter file there are plots in data_val
for(i in unique(data_val$plot_id)){
  year_start <- min(filter(data_val, plot_id==i)$year) # The year of the first inventory
  year_end <- max(filter(data_val, plot_id==i)$year) # The year of the last inventory
  timesteps <- as.numeric(year_end) - as.numeric(year_start) # The gap between the most temporally distant inventories
  growth <- filter(PEP_growth, ID==i) # We keep the growths on this inventory.
  data2 <- filter(data_val, plot_id==i, year==year_start) # Only the PEP data from the first inventory of the validation plot are kept
  # We launch the function that will modify and create a new set of parameters
  create_param_file(data=data2, file_name=paste0(i), growth=growth, timesteps=timesteps, 
                    out_dir=out_dir, par_dir=par_dir, par_name="../data/Parameter_file.xml") 

  # I then open the parameter file to change the time step we want to retrieve the 
  # detailed data
  par_xml <- read_xml(paste0(par_dir, "/",i, ".xml"))
  par_list <- as_list(par_xml)
  
  #I delist and by relisting I change the values.
  par_list2 <- unlist(par_list)
  par_list <- relist(replace(par_list2, names(par_list2)=="paramFile.Output.ou_treeInfo.ou_saveFreq", 1), skeleton=par_list)
  
  par_xml <- as_xml_document(par_list)
  write_xml(par_xml, paste0(par_dir, "/",i, ".xml"))
}

# Execute SORTIE with these 10 parameter files. 