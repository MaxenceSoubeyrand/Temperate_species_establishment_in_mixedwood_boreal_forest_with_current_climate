# Combine all plot results into one table. 

rm(list=ls())

library(tidyverse)

files_adult <- dir("../out_df", full.names = T)

f <- function(x) get(readRDS(x))

res_detailed <- files_adult %>% map(f) %>% reduce(rbind) %>% 
  separate(plot, c("plot_id", "stage")) 

saveRDS(res_detailed, file="../results/res_detailed.rds")