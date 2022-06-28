# We performed a model selection among the 4 model versions (2 residual distributions 
# x 2 models for species competition) based on the approximate leave-one-out 
# cross-validation process (PSIS-LOO method) with the R loo package. 

rm(list=ls())

#setwd à enlever après
setwd("~/PhD/Chap1/Github/parametrisation/growth_altering_effect/code")

library(tidyverse)
library(cmdstanr)
library(rstan)
library(loo)
library(posterior)

# Reading RESEF and LDTRF growth data and neighborhood data
dat <- readRDS("../data/nci_dat.rds") %>% 
  mutate(PLS_PPA=paste0(PLS,"_", PPA))

neighbors <- readRDS("../data/neighbors.rds") %>%
  filter(dist <= 10) %>% # Set dmax = 10 m for NCI calculation
  mutate(PLS_PPA=paste0(PLS,"_", PPA))

# Remove from the dataset the individuals who do not have neighbors.  
ind_no_ngb <- dat$PLS_PPA[!dat$PLS_PPA %in% neighbors$PLS_PPA]

dat <- dat %>% filter(!PLS_PPA %in% ind_no_ngb) %>% 
  select(- PLS_PPA)

neighbors <- neighbors %>% select(-PLS_PPA)


# Get growth data set
growth <- select(dat, PLS, PLACE, PPA, ANNEE, ESS, Diam, Shading) %>%
  # Group data by tree, in order of measurements
  arrange(PPA, PLS) %>%
  group_by(PPA) %>%
  mutate(PLS2 = lead(PLS)) %>% # lead: PLS2 is the following measure
  # by joining this way we match the data of the year and the next year "2"
  inner_join(select(dat, PPA, PLS2 = PLS, ANNEE, Diam), 
             by = c("PPA", "PLS2"), suffix = c("", "2")) %>%
  ungroup()

# For 702A, YEAR2 = 1992 (because October 1988 to May 1993)
growth$ANNEE2[growth$PLS == "702A"] <- 1992

# Calculate the growth rate per year
# Eliminate negative growths and change zeros to 0.005 
# (since the minimum growth that is not zero is ~0.01)
growth <- growth %>%
  mutate(growth_rate = (Diam2 - Diam)/(ANNEE2 - ANNEE)) %>%
  filter(growth_rate >= 0) %>%
  mutate(growth_rate = pmax(growth_rate, 0.005)) 


list_sp <- c("Yellow_Birch", "Red_Maple", "Sugar_Maple", "Balsam_Fir", "Jack_Pine", "Trembling_Aspen", "Paper_Birch", "White_Spruce", "White_Cedar")

# I remove the lines where the Shading is NA (1 for trembling, 1 for paper birch)
PPA_shad_NA <- growth[is.na(growth$Shading) & growth$ESS %in% list_sp,]$PPA

neighbors <- neighbors %>% filter(!PPA %in% PPA_shad_NA) 
growth <- growth %>% filter(!PPA %in% PPA_shad_NA) 

# Creation of table that receive the results of the model parametrisation
par_table <- lambda_table <- data.frame(species=as.character(),
                                        variable=as.character(),
                                        mean=as.numeric(),
                                        median=as.numeric(),
                                        sd=as.numeric(),
                                        mad=as.numeric(),
                                        q5=as.numeric(),
                                        q95=as.numeric(),
                                        rhat=as.numeric(),
                                        ess_bulk=as.numeric(),
                                        ess_tail=as.numeric())

coef_scale <- data.frame(species=as.character(), 
                         mean=as.numeric(),
                         sd=as.numeric(),
                         coef_lin=as.numeric(),
                         coef_quad=as.numeric(),
                         X0=as.numeric())

sp <- list_sp[1]

model_comparison <- NULL

parameter_C <- data.frame(species=as.character(), 
                          C=as.numeric(),
                          q5=as.numeric(),
                          q95=as.numeric())

PG <- data.frame(species=as.character(), 
             iPLS=as.numeric(),
             PLS=as.character(),
             PG=as.numeric())

for(sp in list_sp){ 
  growth_sp <- filter(growth, ESS == sp) %>%
    select(PLS, PPA, Diam, Shading, growth_rate) %>%
    mutate(iPLS = as.integer(as.factor(PLS)),
           iPPA = as.integer(as.factor(PPA)),
           ln_Diam = as.vector(scale(log(Diam))),
           gr = growth_rate)
  growth_sp$iMes <- 1:nrow(growth_sp)
  
  growth %>% filter(grepl("_", ESS)) %>% group_by(ESS) %>% 
    summarize(quant95=quantile(growth_rate,0.95))
  
  ngb_sp <- inner_join(select(growth_sp, iMes, PLS, PPA), neighbors) %>%
    arrange(iMes)
  
  require_suggested_package <- function(x) T
  assert_valid_draws_format <- function(x) T
  samp <- readRDS(paste0("../results/res_model/nci_growth_", sp ,".rds"))
  samp_regroup <- readRDS(paste0("../results/res_model/nci_growth_", sp ,"_regroup.rds"))
  samp_log <- readRDS(paste0("../results/res_model/nci_growth_", sp ,"_log.rds"))
  samp_regroup_log <- readRDS(paste0("../results/res_model/nci_growth_", sp ,"_regroup_log.rds"))
  
  comp <- data.frame(loo_compare(list(samp=samp$loo(), samp_regroup=samp_regroup$loo(), samp_log=samp_log$loo(), samp_regroup_log=samp_regroup_log$loo())))
  
  
  comp$model <- rownames(comp)
  comp$species <- sp
  
  model_comparison <- bind_rows(model_comparison, comp)
  
  samp <- get(comp$model[1]) # samp becomes the samp of the best model. 
  
  draws <- samp$draws(format="matrix") 
  draws <- as_draws_df(draws) 
  
  r_pls <- draws %>% 
    dplyr::select(starts_with("r_pls")) %>% 
    as.matrix()
  
  if(grepl("log", comp$model[1])){
    PG_mat <-  exp(draws$interc  - draws$b_dbh_lin^2 / (4 * draws$b_dbh_quad) + draws$sd_pls * r_pls + draws$sigma^2/2)
  }else{
    PG_mat <-  exp(draws$interc  - draws$b_dbh_lin^2 / (4 * draws$b_dbh_quad)) + draws$sd_pls * r_pls
  }
  
  PG2 <- apply(PG_mat,2, median)
  
  PG_sp <- data.frame(species=sp, 
                   iPLS=1:length(PG2),
                   PG=PG2)
  
  PG <- bind_rows(PG, left_join(PG_sp, unique(select(growth_sp, iPLS, PLS))))
  
  # Table with parameters
  sum <- samp$summary(c("interc", "b_dbh_lin", "b_dbh_quad", "b_shade", "alpha", "beta"))
  sum$species <- sp
  
  par_table <- bind_rows(par_table, sum)
  
  coef_dbh <- samp$draws(c("b_dbh_lin", "b_dbh_quad"), format = "matrix")
  coef_dbh <- as_draws_matrix(coef_dbh) 
  m <- mean(log(growth_sp$Diam))
  s <- sd(log(growth_sp$Diam))
  coef_lin <- coef_dbh[,1]/s - 2*coef_dbh[,2]*m/s^2
  coef_quad <- coef_dbh[,2]/s^2
  Xb_sortie <- sqrt(-0.5/coef_quad)
  colnames(Xb_sortie) <- "Xb"
  X0_sortie <- exp(coef_lin*Xb_sortie^2)
  colnames(X0_sortie) <- "X0"
  sum_X0_Xb <- rbind(summary(Xb_sortie),  summary(X0_sortie))
  sum_X0_Xb$species <- sp
  
  coef_scale <- bind_rows(coef_scale, data.frame(species=sp, 
                                                 mean=m,
                                                 sd=s,
                                                 coef_lin=median(coef_dbh[,1]),
                                                 coef_quad=median(coef_dbh[,2]),
                                                 X0=sum_X0_Xb$median[2]))
  
  par_table <- bind_rows(par_table, sum_X0_Xb)
  
  # Table for lambda
  # Extract lambdas (from parameters -log(lambda))
  nllamb <- samp$draws("nl_lambda", format = "matrix")
  nllamb <- as_draws_matrix(nllamb) 
  lambda <- exp(-nllamb)
  
  if(comp$model[1] %in% c("samp", "samp_log")){
    sp_compet <- sort(c(unique(filter(ngb_sp, ESS_n %in% list_sp)$ESS_n), "OTH"))
    
    colnames(lambda) <- paste0("lambda", "_", sp_compet)
    sum_lambda <- summary(lambda)
    sum_lambda$species <- sp
    }else{#variable effect on species
    colnames(lambda) <- paste0("lambda", "_", c("deciduous", "conifers",sp))
    sum_lambda <- summary(lambda)
    list_sp2 <- list_sp[!list_sp==sp]
    for(sp2 in list_sp2){
      if(sp2 %in% c("Yellow_Birch","Red_Maple", "Sugar_Maple", "Trembling_Aspen", "Paper_Birch")){
        sum_sp <- sum_lambda[1,]
        sum_sp[1,1] <- paste0("lambda", "_", sp2)
        sum_lambda <- rbind(sum_lambda, sum_sp)
      }else{
        sum_sp <- sum_lambda[2,]
        sum_sp[1,1] <- paste0("lambda", "_", sp2)
        sum_lambda <- rbind(sum_lambda, sum_sp)
      }
    }
    sum_lambda <- sum_lambda[-c(1:2),]
    sum_lambda$species <- sp
  }
  lambda_table <- bind_rows(lambda_table, sum_lambda)
  parameter_C <- bind_rows(parameter_C, data.frame(species=sp, 
                                      C=max(sum_lambda$median),
                                      q5=sum_lambda$q5[which.max(sum_lambda$median)],
                                      q95=sum_lambda$q95[which.max(sum_lambda$median)]))
}

#Save the potential growth for correcting biais of parametrisation of climate effect on MaxPotGrowth 
saveRDS(PG, file="../results/paramater_value/potential_growth_resef_ldtrf.rds")

#Save the scale coefficient to apply them on the data of the climate parametrisation
saveRDS(coef_scale, file="../results/paramater_value/coef_scale.rds")

parameter_C2 <- parameter_C %>% 
  mutate_if(is.numeric, signif, 2) %>% 
  mutate(C=paste0(C, "\n(", q5, ", ", q95,")")) %>% 
  select(species, C)

#Save the parameter value for SI
par_table %>% 
  filter(variable %in% c("b_shade", "alpha", "beta", "Xb", "X0")) %>% 
  mutate_if(is.numeric, signif, 2) %>% 
  mutate(med_q=paste0(median, "\n(", q5, ", ", q95,")")) %>% 
  select(species, variable, med_q) %>% 
  pivot_wider(names_from=variable, values_from = med_q) %>% 
  left_join(parameter_C2) %>% 
  rename(Species=species,
         m=b_shade) %>% 
  mutate(Species=str_replace(Species, "_", " ")) %>% 
  select(Species, X0, Xb, m, C, alpha, beta) %>% 
  write.csv(., "../results/paramater_value/growth_parameters.csv", row.names=F)

#Save the lambda value for SI
lambda_table %>% 
  mutate_if(is.numeric, signif, 2) %>% 
  mutate(med_q=paste0(median, "\n(", q5, ", ", q95,")"),
         species=str_replace(species, "_", " "),
         variable=str_replace(substr(variable, 8, nchar(variable)), "_", " ")) %>%
  select(species, variable, med_q) %>% 
  pivot_wider(names_from=variable, values_from = med_q) %>% 
  rename(Species=species) %>% 
  select(-OTH) %>% 
  write.csv(., "../results/paramater_value/lambda_median.csv", row.names = F)

#Save the lambda value for SI and SORTIE modelling
lambda_table %>% 
  select(species, variable, median) %>% 
  group_by(species) %>% 
  mutate(median=median/max(median)) %>% 
  mutate_if(is.numeric, signif, 3) %>% 
  mutate(species=str_replace(species, "_", " "),
         variable=str_replace(substr(variable, 8, nchar(variable)), "_", " ")) %>% 
  pivot_wider(names_from=variable, values_from = median) %>% 
  rename(Species=species) %>% 
  select(-OTH) %>%
  write.csv(., "../results/paramater_value/lambda_median_sortie.csv", row.names = F)

#Save the model selection results for SI
model_comparison %>% relocate(species, model) %>% 
  mutate(species=str_replace(species, "_", " "),
         model=recode(model, 
                      model1="Full",
                      model2="Grouped")) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Species = species,
         Model = model,
         `ELPD difference` = elpd_diff,
         `Standard error difference` = se_diff, 
         ELPD=elpd_loo,
         `Standard error ELPD`=se_elpd_loo) %>% 
  select(-p_loo, -se_p_loo, -looic, -se_looic) %>% 
  write.csv("../results/paramater_value/model_selection.csv", row.names = F)
