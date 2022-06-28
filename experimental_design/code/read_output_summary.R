# We take the summary output, combine them and clean up the variables. 

rm(list=ls())

library(tidyverse)

# setwd à enlever après
setwd("~/PhD/Chap1/Github/experimental_design/code")


source("../../validation/code/process_output_functions.R")

# select all files .out
out_dir <- dir("../results/out", pattern = ".out", full.names = TRUE)

# Execute functions for all files in out
res <- map(out_dir, ~read_summary_output(.))
# Cbind the id of the placette
res <- map2(res, str_remove(out_dir, pattern=".out"), ~cbind(.x, plot_id = .y))
# bind all data of the list in 1 df
res <- bind_rows(res) %>% 
  dplyr::select(-Subplot) %>% 
  relocate(plot_id) %>% 
  rename(Abs.Den=`Abs Den`, Abs.BA=`Abs BA`, Rel.BA=`Rel BA`, Rel.Den=`Rel Den`) %>%
  mutate(plot_id=str_remove(plot_id, pattern="..put/")) %>%
  mutate(plot_id=str_remove(plot_id, pattern=".out")) %>% 
  separate(plot_id, c("fire_year", "species_test", "density"), sep="_") %>%
  mutate(perturbation=ifelse(str_detect(fire_year, "Perturb"), "Perturb", "No_perturb")) %>% 
  mutate(fire_year=str_remove(fire_year, "Perturb")) %>% 
  mutate(year= Step + 1991) %>% 
  mutate_if(is.character,as.factor)
  

# Remove rows with no observations
res_summary  <-  res %>% filter(rowSums(dplyr::select(res, Abs.Den, Abs.BA, Rel.BA, Rel.Den), na.rm=T)!=0) %>% 
  rename(step=Step, stage=Stage, species=Species)

# Save in file
saveRDS(res_summary, file="../results/res_summary.rds")

