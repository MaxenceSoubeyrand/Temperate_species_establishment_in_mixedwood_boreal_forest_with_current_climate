# Script that transforms the pre-model data to run the Bayesian model.
# In this script, we run the full model (all species interaction) with normal distribution of errors. 

rm(list=ls())

library(tidyverse)
library(cmdstanr)

# Reading RESEF growth data and neighborhood data
dat <- readRDS("../data/nci_dat.rds") %>% 
  mutate(PLS_PPA=paste0(PLS,"_", PPA))

deciduous <- c("Sugar_Maple", "Red_Maple", "Trembling_Aspen", "Paper_Birch", "ERE",
             "Yellow_Birch", "ERP", "PRP", "SOA", "Red_Maple", "HEG", "FRA", "TIL",
             "OSV", "PRU", "CET", "CHR", "FRN", "PEG","SAL", "Other")
conifer <- c("EPN", "Jack_Pine", "Balsam_Fir", "White_Spruce", "EPR", "MEL", "PIB", "White_Cedar")

neighbors <- readRDS("../data/neighbors.rds") %>%
    filter(dist <= 10) %>% # Set dmax = 10 m for NCI calculation
    mutate(PLS_PPA=paste0(PLS,"_", PPA)) %>% 
    mutate(ESS_regroup=case_when(ESS_n %in% deciduous ~ "deciduous",
                           ESS_n %in% conifer ~ "conifer"))# new column with conifer/deciduous/intra

neighbors[neighbors$ESS==neighbors$ESS_n,]$ESS_regroup <- "intra"

# Remove from the dataset the individuals who do not have neighbors. 
ind_no_ngb <- dat$PLS_PPA[!dat$PLS_PPA %in% neighbors$PLS_PPA]

dat <- dat %>% filter(!PLS_PPA %in% ind_no_ngb) %>% 
  select(- PLS_PPA)

neighbors <- neighbors %>% select(-PLS_PPA)


# Transform DBH dataset in growth data set
growth <- select(dat, PLS, PLACE, PPA, ANNEE, ESS, Diam, Shading) %>%
  # Group data by tree, in order of measurements
  arrange(PPA, PLS) %>%
  group_by(PPA) %>%
  mutate(PLS2 = lead(PLS)) %>% # lead: PLS2 is the following measure
  # by joining this way we match the data of the year and the next year "2"
  inner_join(select(dat, PPA, PLS2 = PLS, ANNEE, Diam), 
             by = c("PPA", "PLS2"), suffix = c("", "2")) %>%
  ungroup()


# For 702A, ANNEE2 = 1992 (because October 1988 to May 1993)
growth$ANNEE2[growth$PLS == "702A"] <- 1992

# Calculate the growth rate per year
# Eliminate negative growths and change zeros to 0.005 
# (since the minimum growth that is not zero is ~0.01)
growth <- growth %>%
  mutate(growth_rate = (Diam2 - Diam)/(ANNEE2 - ANNEE)) %>%
  filter(growth_rate >= 0) %>%
  mutate(growth_rate = pmax(growth_rate, 0.005)) 

list_sp <- c("Yellow_Birch", "Red_Maple", "Sugar_Maple", "Balsam_Fir", "Jack_Pine", "Trembling_Aspen", "Paper_Birch", "White_Spruce", "White_Cedar")

# Remove the lines where the Shading is NA (1 for trembling, 1 for paper birch)
PPA_shad_NA <- growth[is.na(growth$Shading) & growth$ESS %in% list_sp,]$PPA

neighbors <- neighbors %>% filter(!PPA %in% PPA_shad_NA) 
growth <- growth %>% filter(!PPA %in% PPA_shad_NA) 

#For all species, we transform the data and run the model. 
for(sp in list_sp[1:9]){
  
  # Transform the factors into numerical codes for Stan
  # Also normalize the DHP log and take the growth rate log
  growth_sp <- filter(growth, ESS == sp) %>%
    select(PLS, PPA, Diam, Shading, growth_rate) %>%
    mutate(iPLS = as.integer(as.factor(PLS)),
           iPPA = as.integer(as.factor(PPA)),
           ln_Diam = as.vector(scale(log(Diam))),
           gr = growth_rate)
  growth_sp$iMes <- 1:nrow(growth_sp)
  
  # Filter neighbors for trees in growth_sp
  ngb_sp <- inner_join(select(growth_sp, iMes, PLS, PPA), neighbors) %>%
    arrange(iMes) 
  
  # Number of unique trees in the focal species
  length(unique(growth_sp$PPA))
  
  # Unique trees by neighbor species
  distinct(ngb_sp, ESS_n, PPA_n) %>% count(ESS_n)
  
  ngb_sp$ESS_n[!(ngb_sp$ESS_n %in% list_sp)] <- "OTH"
  ngb_sp$dist[ngb_sp$dist == 0] <- 0.01 # change distances from 0 to 1 cm
  # Transform the species into numerical code for Stan
  ngb_sp$iESS <- as.integer(as.factor(ngb_sp$ESS_n))
  
  # counts the number of neighbors per focal tree
  # and the index of the first and last neighbor for each focal tree
  ngb_count <- count(ngb_sp, iMes)
  last_ngb <- as.integer(cumsum(ngb_count$n))
  first_ngb <- c(1L, last_ngb[1:(length(last_ngb) - 1)] + 1L)
  
  # List of data for the Stan model
  dat_list <- list(n_pls = max(growth_sp$iPLS), n_tree = max(growth_sp$iPPA),
                   n_sp = max(ngb_sp$iESS), n_mes = nrow(growth_sp), n_ngb = nrow(ngb_sp),
                   id_pls = growth_sp$iPLS, id_tree = growth_sp$iPPA,
                   id_sp = ngb_sp$iESS, first_ngb = first_ngb, last_ngb = last_ngb,
                   gr = growth_sp$gr, ln_dbh = growth_sp$ln_Diam, shade = growth_sp$Shading,
                   ngb_dbh = ngb_sp$Diam_n, ngb_dist = ngb_sp$dist)
  
  # Run the model and generate the results
  #Take time (about 10 hours for one species) depending of the number of observations
  mod <- cmdstan_model("nci_growth.stan")
  samp <- mod$sample(data = dat_list, chains = 2, adapt_delta = 0.95)
  samp$save_object(paste0("../results/res_model/nci_growth_",sp,"_regroup.rds"))
}