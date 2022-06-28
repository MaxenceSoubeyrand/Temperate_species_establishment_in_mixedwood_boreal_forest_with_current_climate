# Script for comparing validation data and simulation for trees with ellipses comparison.

rm(list=ls())
# setwd à enlever après
setwd("~/PhD/Chap1/Github/validation/code")

library(RColorBrewer)
library(factoextra)
library(FactoMineR)
library(tidyverse)
theme_set(theme_bw())
library(ggpubr)

#Observations
data_val <- readRDS("../data/PEP_validation_classe.rds")

#Change species names in data validation
colnames(data_val)[2:3] <- c("year", "species")
data_val$species[data_val$species=="BOJ"] <- "Yellow_Birch"
data_val$species[data_val$species=="BOP"] <- "Paper_Birch"
data_val$species[data_val$species=="ERS"] <- "Sugar_Maple"
data_val$species[data_val$species=="ERR"] <- "Red_Maple"
data_val$species[data_val$species=="THO"] <- "White_Cedar"
data_val$species[data_val$species=="PET"] <- "Trembling_Aspen"
data_val$species[data_val$species=="PIG"] <- "Jack_Pine"
data_val$species[data_val$species=="SAB"] <- "Balsam_Fir"
data_val$species[data_val$species=="EPB"] <- "White_Spruce"

#Find the year instead the simulation time
data_year_min <- data_val %>% group_by(plot_id) %>% summarise(year_min=min(year))
data_year_min$plot_id <- as.character(data_year_min$plot_id)
data_year_min$year_min<- as.numeric(data_year_min$year_min)

data_val$year <- as.numeric(data_val$year)

data_val <- data_val %>% mutate(stage=ifelse(dbh_class>10, "Adult", "Sapl")) %>%
  filter(stage=="Adult") %>% 
  mutate(dbh_class=dbh_class-2.5) %>% #pour prendre la moyenne entre la classe sup et la classe inf. 
  group_by(plot_id, year, stage) %>% mutate(dens_plot=sum(density), BA_plot=sum(pi*((dbh_class/200)^2)*density)) %>%
  group_by(plot_id, year, species, stage) %>% 
  summarize(Abs.Den = sum(density),
            Abs.BA = sum(pi*((dbh_class/200)^2)*density), #Pour passer de cm2 ? m2 par hectare
            Rel.Den = sum(density)/mean(dens_plot), 
            Rel.BA = sum(pi*((dbh_class/200)^2)*density)/mean(BA_plot)) %>%
  mutate(plot_id=as.character(plot_id),
         species=as.factor(species),
         stage=as.factor(stage),
         type="Observed",
         subplot=0) %>% 
  select(-Abs.Den, -Rel.BA, -Rel.Den)

year_plot_id_obs <- unique(data_val[,1:2])
year_plot_id_obs <- year_plot_id_obs %>% 
  group_by(plot_id) %>% 
  mutate(serie=1:n())

data_val <- left_join(year_plot_id_obs, data_val)

#Simulated data
data_sim <- readRDS("../results/res_detailed.rds") #Results of the simulation 

data_sim <- data_sim %>%  
  left_join(data_year_min) %>% 
  mutate(timestep=as.numeric(timestep), 
         year=timestep+year_min) %>%
  dplyr::select(-year_min) %>% na.omit()

data_sim <- data_sim %>% 
  group_by(species, year, subplot, plot_id) %>% 
  summarize(Abs.BA=sum((DBH/2/100)^2*pi*25)) %>% 
  mutate(type="Simulated",
         stage="Adult") %>% 
  arrange(plot_id, subplot)


# PCA on the simulated data of the years that are in the validation data
data_sim <- left_join(year_plot_id_obs, data_sim) %>%
  ungroup() %>% 
  filter(!is.na(species))

#Remove pure jack pine stands because PCA buit community with at least 2 species. 
data_sim <- data_sim %>%
  pivot_wider(names_from = species, values_from=Abs.BA, values_fill = 0) %>% 
  filter(!plot_id %in% c("8809410701", "8809411502")) %>% 
  arrange(plot_id, year)

colnames(data_sim)[-1] <- str_replace(colnames(data_sim)[-1], "_", " ")

res.pca <- PCA(data_sim[,7:14],  graph = FALSE)

fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
cercle <- fviz_pca_var(res.pca, repel=T) + 
  theme(text=element_text(size=rel(12)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        title = element_blank(),
        axis.text = element_text(size = 12))

ind <- get_pca_ind(res.pca)$coord

data_sim <- bind_cols(data_sim, data.frame(ind))

# I get the coordinates with the coordinates of the observations  
data_val <- data_val %>%
  pivot_wider(names_from = species, values_from=Abs.BA, values_fill = 0) %>% 
  filter(!plot_id %in% c("8809410701", "8809411502")) %>% 
  arrange(plot_id, year)

colnames(data_val)[-1] <- str_replace(colnames(data_val)[-1], "_", " ")

# Prediction with a newdata (observations)
data_val <- bind_cols(data_val, data.frame(predict(res.pca, newdata=data_val[7:14])$coord)) 

# Get centroid of the simulated points
centroid <- data_sim %>% 
  select(!`Balsam Fir`:`Jack Pine`, -type) %>% 
  group_by(plot_id, year, serie, stage) %>% 
  mutate(centroid_1=mean(Dim.1),
            centroid_2=mean(Dim.2),
            centroid_3=mean(Dim.3),
            centroid_4=mean(Dim.4),
            centroid_5=mean(Dim.5)) %>% 
  # Euclidean distance between the centroid of the simulated and the validation point
  mutate(dist_centroid= sqrt((centroid_1-Dim.1)^2 + 
                               (centroid_2-Dim.2)^2 +
                               (centroid_3-Dim.3)^2 +
                               (centroid_4-Dim.4)^2 +
                               (centroid_5-Dim.5)^2)) %>% 
  group_by(plot_id, year, serie) 

centroid_sim <- centroid %>% 
  summarize(q_low=quantile(dist_centroid, 0.025),
            q_high=quantile(dist_centroid, 0.975),
            q_high95=quantile(dist_centroid, 0.95), 
            median=median(dist_centroid))



# Put the coordinates of the centroids to the observed data

centroid_val <- select(data_val, !`Balsam Fir`:`Jack Pine`, -type, -subplot) %>% 
  left_join(unique(select(ungroup(centroid), plot_id, year,centroid_1, centroid_2, centroid_3, centroid_4, centroid_5))) %>% 
  # Euclidean distance between the centroid of the simulated and the validation point
  mutate(dist_centroid= sqrt((centroid_1-Dim.1)^2 + 
           (centroid_2-Dim.2)^2 +
           (centroid_3-Dim.3)^2 +
           (centroid_4-Dim.4)^2 +
           (centroid_5-Dim.5)^2)) %>% 
  right_join(centroid_sim)


centroid_val <- centroid_val %>% group_by(plot_id) %>% mutate(plot_name=as.factor(cur_group_id()))

# Plot the distance between simulation centroids and simulations, and distance between 
# simulation centroids and observations. If the distance with the observation is 
# within the range of variability of the distance with simulation, we consider as 
# consistent with observation
p_dist_cent <- ggplot(centroid_val, aes(x=plot_name, y=dist_centroid, color=as.factor(serie), ymin=0, ymax=q_high)) + 
  geom_errorbar(position=position_dodge(0.5)) +
  geom_point(position=position_dodge(0.5), size=2) + 
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 11.5),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12)) +
  ylab("Distance to the centroid of simulations") +
  xlab("Validation plots") +
  scale_color_discrete(name="Inventory") +
  annotate("rect", xmin = 4.5, xmax = 5.5, ymin = -0.03, ymax = 3,
          colour = "black", linetype=2, size = 0.70, alpha=0)

# Ellipse in multivariate space for 1 plot.
data_sim_plot <- select(data_sim, plot_id, year, serie, subplot, type, x=Dim.1, y=Dim.2)
data_val_plot <- select(data_val, plot_id, year, serie, subplot, type, x=Dim.1, y=Dim.2)
centroid_val_plot <- select(mutate(centroid_val, type="Centroid"), plot_id, year, serie, type, x=centroid_1, y=centroid_2)
dplot <- bind_rows(data_sim_plot, data_val_plot, centroid_val_plot) %>% 
  mutate(alpha=case_when(type=="Simulated" ~ 0.2,
                         type=="Observed" ~ 1,
                         type=="Centroid" ~ 1),
         size=case_when(type=="Simulated" ~ 0.9,
                         type=="Observed" ~ 1.3,
                         type=="Centroid" ~ 1.3)) %>% 
  filter(plot_id=="7209606102")

dplot$serie <- factor(as.factor(dplot$serie), levels = c("1", "2", "3", "4", "5"))
dplot$type <- factor(as.factor(dplot$type), levels = c("Observed", "Centroid", "Simulated"))

ggplotColours <- function(n=6, h=c(0, 360) +15){
  if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

col <- ggplotColours(5)

p_multi_sp <- ggplot(dplot, aes(x=x, y=y, color=serie, shape=type, alpha=as.factor(alpha), size=as.factor(size))) + 
  geom_point()+
  stat_ellipse(data=filter(dplot, type=="Simulated"), size=0.9, lty="solid", alpha=1) +
  scale_color_manual(name="Inventory", values = c(`1` = col[1], `2` = col[2], `3` = col[3], `4`=col[4], `5`=col[5])) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  guides(alpha="none", size="none") +
  scale_shape_discrete(name="") +
  xlab("Dimension 1 (32.7%)") +
  ylab("Dimension 2  (21.5%)")+ 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.8),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        legend.position="bottom",
        panel.border = element_rect(
          colour = "black",
          linetype=2,
          size = 1)) +
  guides(shape = guide_legend(override.aes = list(size = 3)),
         colour = guide_legend(override.aes = list(size = 2)))

legend <- get_legend(p_multi_sp)

p_droite <- ggarrange(cercle, p_multi_sp + theme(legend.position = "none"), ncol=1, labels = c("(b)", "(c)"))

p <- ggarrange(p_dist_cent + theme(legend.position = "none"), p_droite, ncol=2, widths = c(0.6, 0.4), labels="(a)")

p_final <- ggarrange(p, legend, ncol=1, heights = c(0.95,0.05))

pdf(file = "../results/figures/PCA_centroid.pdf", width = 11.6, height = 9)
  p_final 
dev.off()
# Manipulation with inkscape to move species name to not overlap with arrows (b)

