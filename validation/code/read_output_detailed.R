# Takes the SORTIE results for validation and creates for each plot, a table of 
# results containing, X, Y, species identification, DBH and tree growth every 5 years.

rm(list=ls())
# setwd à enlever après
setwd("~/PhD/Chap1/Github/validation/code")

library(raster)
library(tidyverse)

source("process_output_functions.R")

par_names <- dir("../data/parameter_files", full.names = T)

for(i in par_names){
  placette <- str_remove(i, pattern=".xml")
  placette <- str_remove(placette, pattern="../data/parameter_files/")
  
  # Files containing detailed time results for a replica at all 5 time steps
  t_files <- dir("../results/out", pattern = paste0(placette, "_"), full.names = TRUE)
  step <- str_split(t_files, "_", simplify = TRUE)[,2]
  step <- as.numeric(str_remove(step, ".xml"))
  
  # Produce one data frame per file (treemap; trees only) stored in a list
  t <- map(t_files, ~ treemap_from_file(.)$adult)
  names(t) <- step
  
  t <- compact(t)
  
  # Changes the name of the 5th column of the tables in t.
  for(j in names(t)) {
    colnames(t[[j]])[5] <- paste0("DBH", j)
    colnames(t[[j]])[6] <- paste0("Growth", j)
  }
  
  # Merge the data in a single table with a column for the DBH of each timestep
  t <- t %>%
    reduce(full_join, by=c("species", "X", "Y")) %>%
    dplyr::select(-contains("id"))
  
  # Reverse the board to make it tidy
  t <- t %>% pivot_longer(cols=4:ncol(t), 
                          names_to = c(".value", "timestep"), 
                          names_pattern = "([a-zA-Z]+)(\\d+)")
  
  # Put in a sub-plot the size of the government plots
  r <- raster(ncol=10, nrow=10, xmn=0, xmx=200, ymn=0, ymx=200)
  values(r) <- 1:ncell(r)
  t <- t %>% mutate(subplot=raster::extract(r, SpatialPoints(cbind(t$X, t$Y)))) %>%
    arrange(timestep, subplot)
  t$plot <- paste0(placette, "_adult")
  
  # Creates one result file per plot
  saveRDS(t, file=paste0("../results/out_df/", placette, ".rds"))
}
