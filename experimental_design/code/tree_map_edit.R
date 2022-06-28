#Script we create .txt with X Y diam for each species from the tree map created 
# by SORTIE of the controls, we add the seedlings by creating the tree maps for 
# the scenarios (first loop).
# Then we create as many parameter files as there are tree maps and in each parameter
# file we reference the corresponding tree_maps. 

rm(list=ls())

# setwd à enlever après
setwd("~/PhD/Chap1/Github/experimental_design/code")

library(xml2)
library(tidyverse)

source("../../validation/code/process_output_functions.R")
source("../../validation/code/edit_param_functions.R")

#Create tree maps ----
t_files <- dir("../data/tree_maps_control", pattern = "control", full.names = TRUE)

size_plot <- 100 #length of the plot in meter

low <- 500; medium <- 5000; high <- 10000
density_seedling_data <- data.frame(species_test = c("control", "control", "control",
                                                     "Yellow_Birch", "Yellow_Birch", "Yellow_Birch","Yellow_Birch", "Yellow_Birch", "Yellow_Birch","Yellow_Birch", "Yellow_Birch", "Yellow_Birch",
                                                     "Sugar_Maple", "Sugar_Maple", "Sugar_Maple","Sugar_Maple", "Sugar_Maple", "Sugar_Maple","Sugar_Maple", "Sugar_Maple", "Sugar_Maple",
                                                     "Red_Maple", "Red_Maple", "Red_Maple","Red_Maple", "Red_Maple", "Red_Maple","Red_Maple", "Red_Maple", "Red_Maple",
                                                     "all", "all", "all","all", "all", "all","all", "all", "all"),
                                    density_test = c("NA", "NA", "NA",
                                                     "low", "low", "low", "medium", "medium", "medium", "high", "high", "high",
                                                     "low", "low", "low", "medium", "medium", "medium", "high", "high", "high",
                                                     "low", "low", "low", "medium", "medium", "medium", "high", "high", "high",
                                                     "low", "low", "low", "medium", "medium", "medium", "high", "high", "high"),
                                    species_id = c("Yellow_Birch", "Sugar_Maple", "Red_Maple",
                                                   "Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple",
                                                   "Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple",
                                                   "Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple",
                                                   "Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple","Yellow_Birch", "Sugar_Maple", "Red_Maple"),
                                    density = c(0, 0, 0,
                                                low, 0, 0, medium, 0, 0, high, 0, 0,
                                                0, low, 0, 0, medium, 0, 0, high, 0,
                                                0, 0, low, 0, 0, medium, 0, 0, high,
                                                low, low, low, medium, medium, medium, high, high, high))

# We remove the control since the tree map is already created 
# (SORTIE the already created)
density_seedling_data <- density_seedling_data %>% 
  filter(species_test!="control")

for(i in t_files){ 
  map <- treemap_from_file(i) 
  map[["seedling"]]$DBH <- 0.1
  map <- bind_rows(map, .id="Type")

  map <- map %>% 
    mutate(Height=0) %>% 
    rename(Species=species, Diam=DBH) %>% 
    dplyr::select(X, Y, Species, Type, Diam, Height) %>% 
    mutate(Type=str_to_sentence(Type))
  
  fire_year <- str_split(i, pattern = "/")[[1]][4]
  fire_year <- str_split(fire_year, pattern = "_")[[1]][1]
  
  tree_map_control_name <- paste0(str_remove(str_split(i, pattern = "/")[[1]][4], "_0.xml"), ".txt")
  
  write.table(x = map, file = paste0("../data/tree_maps/", tree_map_control_name), sep="\t", row.names=F, quote=F)
  
  for(j in seq(1, nrow(density_seedling_data), 3)){
    sub_dens <- density_seedling_data[j:(j+2),]

    tree_map_name <- paste0(fire_year, "_", case_when(sub_dens[1,1]=="Yellow_Birch" ~ "BOJ",
                                                      sub_dens[1,1]=="Sugar_Maple" ~ "ERS",
                                                      sub_dens[1,1]=="Red_Maple" ~ "ERR",
                                                      sub_dens[1,1]=="all" ~ "all"), "_", sub_dens[1,2], ".txt")
    
    map2 <- map
    
    map_seed <- data.frame(X=as.numeric(), Y=as.numeric(), Species=as.character(), Type=as.character(), Diam=as.numeric(), Height=as.numeric())
    for(k in 1:3){
      map_seed <- rbind(map_seed, 
                        data.frame(X=runif(n=sub_dens$density[k], min = 0, max=size_plot), 
                                   Y=runif(n=sub_dens$density[k], min = 0, max=size_plot), 
                                   Species=rep(sub_dens$species_id[k], times=sub_dens$density[k]), 
                                   Type=rep("Seedling", times=sub_dens$density[k]), 
                                   Diam=rep(0.1, times=sub_dens$density[k]), 
                                   Height=rep(0, times=sub_dens$density[k])))
    }
    map2 <- rbind(map2, map_seed)
    
    write.table(x = map2, file = paste0("../data/tree_maps/", tree_map_name), sep="\t", row.names=F, quote=F)
  }
}

#Create parameter files ----
# The tree_maps (.txt) are correct now, we take the control parameter file of each
# scneario, add dummy cond inits to it, and reference the pathway of the tree map
t_files <- dir("../data/tree_maps/", full.names = F)
timesteps <- 109

# We can't put the tree_maps AND the initial densities, so we have to set all densities 
# to zero
# So I create an empty density array that will update the parameter file to have 
# no trees at the beginning but the tree in tree maps.
dens_init_empty <- data.frame(plot_id="1760", year=1991, 
                             species_id="ABA", dbh_class=10, density=0)

# For each tree maps
for(i in t_files){ #i=t_files[1]
  # Read the correct parameter file
  par_file_control <- paste0(str_split(i, "_")[[1]][1], "_control_NA.xml")
  par_xml <- read_xml(paste0("../data/parameter_files/", par_file_control))
  par_list <- as_list(par_xml)
  
  #Name of the parameter file
  par_name <- substr(i,1,nchar(i)-4)
  
  new_par_list <- par_list
  #Add a tree map file:
  new_par_list$paramFile$trees$tr_treemapFile[[1]] <- paste0("/folder/where/the/tree/maps/are", i) #folder where the tree maps are
  
  # Change number of timesteps
  new_par_list$paramFile$plot$timesteps[[1]] <- as.character(timesteps)
  
  #Change the output:
  new_par_list$paramFile$Output$ou_filename[[1]] <- paste0("/folder/where/the/simulation/output/will/be/stocked", par_name, ".gz.tar")
  new_par_list$paramFile$ShortOutput$so_filename[[1]] <- paste0("/folder/where/the/simulation/output/will/be/stocked", par_name, ".out")
  
  #remplace initial density by nothing!
  new_par_list$paramFile$trees$tr_initialDensities <- get_transect_init_dens(data=dens_init_empty)
  
  par_xml_new <- as_xml_document(new_par_list)
  write_xml(par_xml_new, paste0("../data/parameter_files/", par_name, ".xml"))
}

# Execute SORTIE with the parameter files and tree maps created.
# We execute SORTIE on digital research alliance of Canada computers.