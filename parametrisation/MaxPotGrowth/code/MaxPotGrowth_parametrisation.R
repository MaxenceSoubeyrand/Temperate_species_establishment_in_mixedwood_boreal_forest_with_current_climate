# Script that give the MaxPotGrowth parameter value of the species with the climate of the LDRTF:
# -Find climatics variable in PEP and LDRTF
# -Open the data of Anthony, 
# -Compute the growth climate relationship
# -Compute growth for the initial conditions --> PEP and LDRTF


rm(list=ls())


setwd("~/PhD/Chap1/Temperate_species_establishment_in_mixedwood_boreal_forest_with_current_climate/parametrisation/MaxPotGrowth/code")

library(tidyverse)
theme_set(theme_bw())
library(quantreg)
library(brms)

# LDRTF climate data
LDRTF_clim_brut <- read.csv("../data/LDRTF_climateNA_1970-2019MSY.csv")
colnames(LDRTF_clim_brut)[2] <- "ID1"

LDRTF_clim <- LDRTF_clim_brut %>% 
  mutate(CMI_JJA=rowSums(LDRTF_clim_brut[c("CMI06",	"CMI07",	"CMI08")])) %>%
  group_by(Latitude, Longitude, Elevation) %>%
  summarise(CMI_JJA = mean(CMI_JJA),
            MSP=mean(MSP),
            DD5=mean(DD5),
            MAT=mean(MAT)) %>%
  select(Latitude, Longitude, CMI_JJA, MSP, DD5, MAT)

# Validation site climate data
PEP_clim <- read.csv("../data/PEP_climateNA_1970-2019MSY.csv")

PEP_clim <- PEP_clim %>% 
  mutate(CMI_JJA=rowMeans(PEP_clim[c("CMI06",	"CMI07",	"CMI08")]))%>%
  group_by(ID1, Latitude, Longitude, Elevation) %>%
  summarise(CMI_JJA = mean(CMI_JJA), MSP=mean(MSP), MAT=mean(MAT), DD5=mean(DD5)) %>%
  select(ID1, Latitude, Longitude, CMI_JJA, MSP, MAT, DD5)

# RESEF climate data
RESEF_clim <- read.csv("../data/clim_resef_1986-2017MSY.csv") %>% 
  mutate(ID1=as.character(ID1))

LDRTF_clim_spatial <- bind_rows(mutate(LDRTF_clim_brut, ID1="H1760"),
                               mutate(LDRTF_clim_brut, ID1="H1823"),
                               mutate(LDRTF_clim_brut, ID1="H1847"),
                               mutate(LDRTF_clim_brut, ID1="H1870"),
                               mutate(LDRTF_clim_brut, ID1="H1916"),
                               mutate(LDRTF_clim_brut, ID1="H1944"))

spatial_clim <- bind_rows(LDRTF_clim_spatial, RESEF_clim)


spatial_clim <- spatial_clim %>% 
  mutate(CMI_JJA=rowMeans(spatial_clim[c("CMI06",	"CMI07",	"CMI08")]))%>%
  mutate(ID1=paste0(ID1, "_", Year)) %>% 
  select(ID1, Latitude, Longitude, CMI_JJA, MSP, MAT, DD5)


# Merge LDRTF RESEF and PEP to compute in the same loop.
LDRTF_clim$ID1 <- "LDRTF"
PEP_clim$ID1 <- as.character(PEP_clim$ID1)

LDRTF_PEP_clim <- rbind(spatial_clim, PEP_clim, LDRTF_clim)

# Opening the PSP dataset
# PSP dataset is a dataset which contains the growth of species in North America,
# Growth data come from government plots.

# Species code
# sugar maple (Acer saccharum) = 318
# red maple (Acer rubrum) = 316
# yellow birch (Betula alleghaniensis) = 371
# paper birch (Betula papyrifera) = 375
# trembling aspen (Populus tremuloides) = 746
# white cedar (Thuja occidentalis) = 241
# white spruce (Picea glauca) = 94
# balsam fir (Abies balsamea) = 12
# jack pine (Pinus banksiana) = 105

PSP <- readRDS(file="../data/growth_clim_PSP.rds")

code_esp <- data.frame(esp=c("Sugar_Maple","Red_Maple", "Yellow_Birch","Paper_Birch",
                             "Trembling_Aspen", "White_Cedar", "White_Spruce",
                             "Balsam_Fir","Jack_Pine"),
                       code=c(318, 316, 371, 375, 746, 241, 94, 12, 105))

PSP <- PSP %>% filter(MAT!=-9999)%>%
   left_join(code_esp, by = c("SpeciesFIA"="code"))



# Scale climate variables, DBH is centered reduce per species in the loop with the 
# coefficients from RESEF and LDRTF data set
PSP_norm <- PSP  %>%
  mutate(MAT=scale(MAT),
         MSP=scale(MSP),
         CMI_JJA=scale(CMI_JJA),
         DD5=scale(DD5)) 

# Coeffcient to scale DBH with same value of the parametrisation of growth altering 
# effects
coef_scale <- readRDS("../../growth_altering_effect/results/paramater_value/coef_scale.rds")

# Normalization with the mean and sd of the growth file
LDRTF_PEP_clim_norm <- LDRTF_PEP_clim %>%
  mutate(MAT=(MAT-mean(PSP$MAT))/sd(PSP$MAT),
         MSP=(MSP-mean(PSP$MSP))/sd(PSP$MSP),
         DD5=(DD5-mean(PSP$DD5))/sd(PSP$DD5),
         CMI_JJA=(CMI_JJA-mean(PSP$CMI_JJA))/sd(PSP$CMI_JJA))



# Initialise table for results
# Table for the growth of species in the sites
LDRTF_PEP_growth <- data.frame(ID=as.character(), 
                               species=as.character(),
                               growth=as.numeric(),
                               error_growth=as.numeric(),
                               X0=as.numeric(),
                               Xb=as.numeric())

# Table for growth curves
response_curve_data <- data.frame(species=as.character(),
                                  fix_eff=as.character(),
                                  x=as.numeric(),
                                  y=as.numeric(),
                                  se=as.numeric())

# Table to receive the parameter values
summary_tab <- data.frame()

# loop on species
for(i in code_esp$code){
  PSP_esp <- filter(PSP_norm, SpeciesFIA==i)
  
  
  coef_scale_sp <- filter(coef_scale, species==PSP_esp$esp[1]) 
  PSP_esp$ldbh <- (log(PSP_esp$DBH) - coef_scale_sp$m) / coef_scale_sp$sd
  
  PSP_esp$dbh_effect <- coef_scale_sp$coef_lin * PSP_esp$ldbh + coef_scale_sp$coef_quad * PSP_esp$ldbh^2
  
  # Quantile bayesian model on the growths of PSP data as a function of the 4 climate 
  # variables and the DBH effect calculated in the first parameterization. 
  # brq <- brm(bf(log(DBHI) ~ interc + lin + quad , quantile = 0.95, nl = TRUE,
  #               interc ~ 1, lin ~ 0 + MAT + MSP + DD5 + CMI_JJA + offset(dbh_effect),
  #               quad ~ 0  + I(MSP^2) + I(MAT^2) + I(DD5^2) + I(CMI_JJA^2)),
  #            prior = c(prior(normal(0, 1), nlpar = interc),
  #                      prior(normal(0, 1), class = b, nlpar = lin),
  #                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
  #            data = PSP_esp, family = asym_laplace(), chains = 2)
  # 
  # saveRDS(brq, paste0("../results/res_clim_mod/brq_res_", PSP_esp$esp[1],".rds"))
  
  brq <- readRDS(paste0("../results/res_clim_mod/brq_res_", PSP_esp$esp[1],".rds"))
  summary_tab_sp <- mutate(data.frame(fixef(brq)), species=PSP_esp$esp[1], effect=rownames(data.frame(fixef(brq))))
  
  summary_tab <- bind_rows(summary_tab, summary_tab_sp)
  
  #Prédiction avec les données des PEP et LDRTF
  X0 <- (log(coef_scale_sp$X0) - coef_scale_sp$m) / coef_scale_sp$sd #remplacer par X0
  LDRTF_PEP_clim_norm$dbh_effect <- coef_scale_sp$coef_lin * X0 + coef_scale_sp$coef_quad * X0^2
  fit <- fitted(brq, newdata=LDRTF_PEP_clim_norm, dpar="mu")
  
  LDRTF_PEP_growth <- rbind(LDRTF_PEP_growth,
                            data.frame(ID=LDRTF_PEP_clim_norm$ID1, 
                                       species=PSP_esp$esp[1],
                                       growth=exp(fit[,1]),
                                       error_growth=exp(fit[,2])))
  
  #tableau pour les courbes de croissances
  cond_eff <- conditional_effects(brq, dpar="mu")
  for(j in c("MAT", "MSP", "DD5", "CMI_JJA")){
    response_curve_data <- rbind(response_curve_data,
                                 data.frame(species=PSP_esp$esp[1],
                                            fix_eff=j,
                                            x=cond_eff[[j]][["effect1__"]],
                                            y=cond_eff[[j]][["estimate__"]],
                                            se=cond_eff[[j]][["se__"]]))
  }
}

tibble(summary_tab) %>% 
  mutate(species=str_replace(species, "_", " ")) %>%
  mutate_if(is.numeric, signif, 2) %>% 
  mutate(Estimate=paste0(Estimate,"\n(", Q2.5, ",", Q97.5, ")")) %>% 
  select(Species=species, Effects=effect, Estimate) %>% 
  pivot_wider(names_from = Effects, values_from = Estimate) %>% 
  write.csv(file="../results/MaxPotGrowth_clim_effect.csv", row.names = F)
  

# Calculation of the bias
spatial_growth <- LDRTF_PEP_growth %>% 
  filter(ID %in% spatial_clim$ID1)

place <- readRDS("../../growth_altering_effect/data/nci_dat.rds") %>% 
  select(PLS, PLACE, ANNEE) %>% 
  mutate(ANNEE=as.factor(ANNEE))

growth_mod <- readRDS("../../growth_altering_effect/results/paramater_value/potential_growth_resef_LDRTF.rds") 

growth_clim <- spatial_growth %>% 
  separate(ID, c("PLACE", "ANNEE")) %>% 
  left_join(unique(place)) %>% 
  filter(!is.na(PLS)) %>% 
  left_join(growth_mod) %>% 
  filter(!is.na(PG)) %>% 
  arrange(PLACE, species) %>% 
  mutate(log_growth=log(growth), log_PG=log(PG))

ggplot(growth_clim, aes(x=log_PG, y=log_growth)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~species) +
  xlab("Growth with the RESEF data model") +
  ylab("Growth with the quantile model")

bias <- growth_clim %>% 
  group_by(species) %>% 
  summarize(bias=mean(log_PG-log_growth))

saveRDS(bias, "../results/bias_log_MaxPotGrowth.rds")

growth_clim <- growth_clim %>% 
  group_by(species) %>% 
  mutate(bias=mean(log_PG-log_growth),
         log_growth_no_bias=log_growth+bias)

ggplot(growth_clim, aes(x=log_PG, y=log_growth_no_bias)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~species) +
  xlab("Growth with the RESEF data model") +
  ylab("Growth with the quantile model")
  
# Remove plots from RESEF from here. Re calculation of maximum potential growth 
# with PSP data set.  
LDRTF_PEP_growth <- LDRTF_PEP_growth %>% 
  filter(!ID %in% spatial_clim$ID1)

LDRTF_PEP_growth <- LDRTF_PEP_growth %>% 
  left_join(bias) %>% 
  mutate(log_growth_no_bias=log(growth)+bias) %>% 
  mutate(growth_no_bias=exp(log(growth)+bias))

ggplot(data=LDRTF_PEP_growth, aes(x=species, y=growth_no_bias)) + geom_boxplot() + ylim(0, NA) +
  ylab("MaxPotGrowth (in cm)")
ggplot(data=LDRTF_PEP_growth, aes(x=species, y=log_growth_no_bias)) + geom_boxplot() +
  ylab("MaxPotGrowth (in cm)")


saveRDS(LDRTF_PEP_growth, file="../results/LDRTF_PEP_MaxPotGrowth.rds")

#Plot of maximum growth of species at DHP=X0
LDRTF_PEP_growth$species <- str_replace(LDRTF_PEP_growth$species, "_", "\n")

png(filename = "../results/MaxPotGrowth.png", width = 650, height = 400)
ggplot(data=filter(LDRTF_PEP_growth, species!="Jack\nPine"), aes(x=species, y=growth_no_biais)) + geom_boxplot() + 
  #geom_point(data=filter(LDRTF_PEP_growth, ID=="LDRTF"), aes(x=species, y=growth15cm)) +
  ylab("Maximum Potential Growth (in cm)") + xlab("") +
  ylim(0,NA) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))
dev.off()

#Plot des courbes de croissances
LDRTF <- filter(LDRTF_PEP_clim_norm, ID1=="LDRTF") %>% ungroup() %>% select(CMI_JJA, MSP, MAT, DD5) %>% t()
LDRTF <- data.frame(x=as.numeric(LDRTF[,1]), fix_eff=rownames(LDRTF))
LDRTF$fix_eff <-  str_replace(LDRTF$fix_eff, "_", " ")

response_curve_data$species <- str_replace(response_curve_data$species, "_", " ")
response_curve_data$fix_eff = factor(response_curve_data$fix_eff, 
                                     levels=c("ldbh", "CMI_JJA", "DD5", "MAT", "MSP"),
                                     labels=c("logDBH", "CMI JJA", "DD5", "MAT", "MSP"))
PSP_norm2 <- PSP_norm %>% 
  pivot_longer(CMI_JJA:DD5, names_to="fix_eff", values_to="x") %>% 
  select(species=esp, fix_eff, x=x, y=DBHI)


png(filename = "../results/MaxPotGrowth_climate_relationship_temp_species.png", width = 600, height = 380)
ggplot(data=filter(response_curve_data, species %in% c("Sugar Maple", "Red Maple", "Yellow Birch")), aes(x=x, y=exp(y), ymin=exp(y-se), ymax=exp(y+se))) + 
  geom_line() + geom_ribbon(alpha=0.2) +
  facet_grid(species~fix_eff, scales="free_x") +
  ylab("MaxPotGrowth (in cm)") + xlab("fixed scale effect")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12)) +
  geom_vline(data=LDRTF, aes(xintercept=x), linetype="dashed") + 
  ylim(0,NA)
dev.off()