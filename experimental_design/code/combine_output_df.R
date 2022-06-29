# Combine all plot results into one table. 

rm(list=ls())

library(tidyverse)

files_adult <- dir("../results/out_df", full.names = T)

f <- function(x) get(load(x))

res_detailed_adult <- files_adult %>% map(f) %>% reduce(rbind) %>% 
  separate(plot, c("fire_year", "species_test", "density", "stage")) %>%
  mutate(perturbation=ifelse(str_detect(fire_year, "Perturb"), "Perturb", "No_perturb")) %>% 
  mutate(fire_year=str_remove(fire_year, "Perturb"))

saveRDS(res_detailed_adult, file="../results/res_detailed.rds")
