# Creation of the control parameter files, the different temperate hardwood scenarios 
# added are created in the file tree_map_edit.R.
# initial condition come from the data paper from Maleki et al 2021.

rm(list=ls())


# setwd à enlever après
setwd("~/PhD/Chap1/Github/experimental_design/code")

library(tidyverse)
library(xml2)


# Load functions
source("../../validation/code/edit_param_functions.R")

# Creation of controls here only.
# Here we create the parameter files to make the tree map. 

# Initial conditions from LDRTF data:
data <- read.csv("../data/transect_meas.csv")
data$plot_id[data$plot_id=="1919-T1-0-50"] <- "1919-T1-0050"
data$plot_id[data$plot_id=="1919-T1--100"] <- "1919-T1-0100"

data_dead_balsam_fir <- data %>% separate(plot_id, c("Feux", "Transect", "id"), sep="-") %>%
  filter(year=="1991",
         status_id!="A",
         Feux != "1919",
         species_id %in% c("ABA"))


data <- data %>% separate(plot_id, c("Feux", "Transect", "id"), sep="-") %>%
  filter(year=="1991",
         status_id=="A",
         Feux != "1919",
         species_id %in% c("TOC", "ABA", "PGL", "PBA", "PTR", "BPA", "ERS", "ERR", "BOJ")) %>% 
  rbind(data_dead_balsam_fir) %>% # Consider dead Balsam fir as alive
  arrange()

data <- left_join(data, unique(data[,1:3]) %>% group_by(Feux) %>% 
                    summarize(nb_replicats=n())) %>% 
  #Calculation of the basal area per row of the whole table
  mutate(ST=count*(dbh_class*0.01/2)^2*pi, 
         size_plot=case_when(dbh_class == 0.5 ~ 12,
                             dbh_class == 2.5 ~ 64,
                             TRUE ~ 256),
         #Area sampled per hectare 256m2/1h (plots are 256 m2)
         area=nb_replicats*size_plot/10000) %>%  #Aire échantillonné par hectare 256m2/1h (les plots font 256 m2)
  group_by(Feux, year, species_id, dbh_class, area) %>%
    summarize(ST=sum(ST),
              density=sum(count)) %>% 
  mutate(ST=ST/area,
         density=density/area, 
         dbh_class = ifelse(dbh_class>0.5, dbh_class+2.5, dbh_class)) %>% 
  select(-area, plot_id=Feux) %>% 
  filter(plot_id %in% c("1760", "1797", "1823",  "1870", "1916", "1964",
                        "1760e", "1797e", "1823e",  "1870e", "1916e", "1964e"))

data_ST <- data %>% 
  group_by(plot_id, species_id) %>% 
  summarize(ST=sum(ST))

#Get the MaxPotGrowth of species in LDRTF
LDRTF_PEP_growth <- readRDS("../../parametrisation/MaxPotGrowth/results/LDRTF_PEP_MaxPotGrowth.rds")
LDRTF_growth <- filter(LDRTF_PEP_growth, ID=="LDRTF") %>% 
  select(ID, species, growth_no_bias) %>% 
  mutate(species=str_replace(species,"\n", "_"))

out_dir <- "/path/where/output/will/be/stocked" # Where the output will be directed.
par_dir <- "../data/parameter_files" # Where the parameter files will be directed

timesteps <- 1
year_start <- 1991 

# Unharvested stands
# Create as many parameter file there are plots in data
for(i in unique(data$plot_id)){ 
  ####Control --> without temperate Hardwoods !!!!
  data2 <- filter(data, plot_id==i, year==year_start)
  #NA because there is no seeding rate since these are the controls
  file_name <- paste0(i,"_control_NA") 
  create_param_file(data=data2, file_name=file_name, growth=LDRTF_growth, 
                    timesteps=timesteps, out_dir=out_dir, par_dir=par_dir, par_name="../../validation/data/Parameter_file.xml") 

  #I then open the parameter file to change the plot size
  par_xml <- read_xml(paste0(par_dir, "/", file_name, ".xml"))
  par_list <- as_list(par_xml)
  
  par_list[["paramFile"]][["plot"]][["plot_lenX"]][[1]] <- 100
  par_list[["paramFile"]][["plot"]][["plot_lenY"]][[1]] <- 100
  
  par_xml <- as_xml_document(par_list)
  write_xml(par_xml, paste0(par_dir, "/", file_name, ".xml"))
}

# Harvested stands
# data from Brais, S., Work, T., Robert, É., O'Connor, C., Strukelj, M., Bose, A.,
# Celentano, D., & Harvey, B. (2013). Ecosystem Responses to Partial Harvesting 
# in Eastern Boreal Mixedwood Stands. Forests, 4(2), 364-385. https://doi.org/10.3390/f4020364
har_IC <- readRDS("../data/harvested_initial_conditions.rds")

timesteps <- 1
out_dir <- "/path/where/output/will/be/stocked" # Where the output will be directed.
par_dir <- "../data/parameter_files" # Where the parameter files will be directed

for(i in unique(har_IC$plot_id)){ 
  ####Control --> without temperate Hardwoods
  semis2 <- filter(har_IC, plot_id==i)
  file_name <- paste0(i,"_control_NA") 
  create_param_file(data=semis2, file_name=file_name, growth=LDRTF_growth, 
                    timesteps=timesteps, out_dir=out_dir, par_dir=par_dir, par_name="../../validation/data/Parameter_file.xml") 
  
  par_xml <- read_xml(paste0(par_dir, "/", file_name, ".xml"))
  par_list <- as_list(par_xml)
  
  par_list[["paramFile"]][["plot"]][["plot_lenX"]][[1]] <- 100
  par_list[["paramFile"]][["plot"]][["plot_lenY"]][[1]] <- 100
  
  par_xml <- as_xml_document(par_list)
  write_xml(par_xml, paste0(par_dir, "/", file_name, ".xml"))
}

# Execute these 8 parameter file for 1 timestep to get the tree map.
# Take the parameter_file that SORTIE create before the first timestep
# and stock them in tree_maps_control folder.
# The tree map for each stands will be used to test the scenarios. 
# Code to get tree map from simulations and create parameter file for the scenarios
# is tree_map_edit.R