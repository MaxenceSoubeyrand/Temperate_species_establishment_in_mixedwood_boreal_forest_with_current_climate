# Script to build figure 4 of the article. Basal area of temperate species as a 
# function of simulated scenarios.
rm(list=ls())

library(tidyverse)
library(ggpubr)
theme_set(theme_bw())

res <- readRDS(file="../results/res_detailed.rds")

nb_subplot <- 25


res_mean <- res %>% 
  # Calculation of basal area in each subplot for each species
  group_by(fire_year, species_test, density, subplot, species, timestep, perturbation) %>% 
  summarise(abs.BA=sum((DBH/2/100)^2*pi)*25) %>% 
  group_by(fire_year, species_test, density, species, timestep, perturbation) %>% 
  # All of this for quantile so that when the species is not there we put zero rather than the absence of line
  summarise(mean_abs.BA=sum(abs.BA)/nb_subplot,
            q_low=quantile(if(length(abs.BA) != nb_subplot){c(abs.BA, rep(0, nb_subplot-length(abs.BA)))}else{abs.BA} , 0.025), #tout ce micmac pour que quand l'espèce n'est pas là on mette zéro plutôt que l'absence de ligne
            q_high=quantile(if(length(abs.BA) != nb_subplot){c(abs.BA, rep(0, nb_subplot-length(abs.BA)))}else{abs.BA} , 0.975)) %>% 
  mutate(year=as.numeric(timestep)+1991) %>% 
  filter(year==2100) %>% 
  droplevels() %>% 
  filter(!species %in% c("Jack_Pine", "Balsam_Fir", "Paper_Birch",
                         "Trembling_Aspen", "White_Cedar",
                         "White_Spruce"))

# Change labels of factor to have correct labels figure and in the good order
res_mean$species_test <- factor(res_mean$species_test, 
                                levels = c("control", "BOJ", "ERS", "ERR", "all"), 
                                labels = c("Control", "Yellow Birch", "Sugar Maple", "Red Maple", "All"))

res_mean$species <- factor(res_mean$species, 
                           levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                      "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                      "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                           labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                      "Red Maple", "Sugar Maple", "Trembling Aspen",
                                      "White Cedar", "White Spruce", "Yellow Birch"))



res_mean$density <- factor(res_mean$density, 
                           levels = c("NA", "low","medium","high"), 
                           labels=c("None", "Low","Medium","High"))


#random color + alpha
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

res_mean$fire_year <- factor(res_mean$fire_year, 
                             levels = c("1760", "1797", "1823", "1870", "1916", "1964", "1910", "1923"))

res_mean$perturbation <- factor(res_mean$perturbation, 
                                levels = c("No_perturb", "Perturb"), 
                                labels = c("Unharvested", "Harvested"))


plot_1_1 <- ggplot(filter(res_mean, species_test != "All"), 
                   aes(x=density, 
                       y=mean_abs.BA,  
                       color=fire_year, 
                       ymin=q_low, 
                       ymax=q_high)) +
  geom_point(position=position_dodge(0.6), size=2) +
  geom_errorbar(width=0.2, position=position_dodge(0.6), size=1) +
  facet_grid(~species, scales = "free_x", space = "free_x") +
  scale_x_discrete(name ="Density") +
  theme(plot.caption = element_text(hjust = 0)) + 
  ylab(expression("Adult basal area at time 109 years "~(m^2/h)))+
  scale_color_discrete("Fire year") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  ggtitle(expression(paste("Adding ", bold("one"), " temperate tree species")))


plot_all <- ggplot(filter(res_mean, species_test == "All"), 
                   aes(x=density, y=mean_abs.BA,  color=fire_year, ymin=q_low, ymax=q_high)) +
  geom_point(position=position_dodge(0.6), size=2) +
  geom_errorbar(width=0.2, position=position_dodge(0.6), size=1) +
  facet_grid(~species, scales = "free_x", space = "free_x") +
  scale_x_discrete(name ="Density") +
  theme(plot.caption = element_text(hjust = 0)) + 
  ylab(expression("Adult basal area at time 109 years "~(m^2/h)))+
  scale_color_discrete("Fire year") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  ggtitle(expression(paste("Adding ", bold("all"), " temperate tree species"))) +
  ylim(0, max(res_mean$q_high)) 


# Combine nicely plot_1_1 and plot_all and save it
pdf(file = "../results/figures/BA_temperate_1_1_and_all.pdf", width = 10, height = 9, onefile=F)
  ggarrange(plot_1_1, plot_all, common.legend = T, nrow=2, legend="bottom", labels = c("(a)", "(b)"))
dev.off()

# Dashed frame to separate harvested and non-harvested stands has been added after 
# with inkscape software
