# Script that allows to find the figure 6 of the manuscript. We did a PCA on the 
# results of the simulations, then we displayed the average trajectories of the scenarios,
#we added a point for the initial conditions and a point for the final condition

rm(list=ls())

# setwd à enlever après
setwd("~/PhD/Chap1/Temperate_species_establishment_in_mixedwood_boreal_forest_with_current_climate/experimental_design/code")


library(tidyverse)
theme_set(theme_bw())
library(FactoMineR)
library(factoextra)
library(gtable)
library(grid)

res <- readRDS(file="../results/res_detailed.rds")

res <- res %>% 
  #Calculation of BA in each subplot for each species
  group_by(fire_year, species_test, density, subplot, species, timestep, perturbation) %>% 
  summarise(abs.BA=sum((DBH/2/100)^2*pi)*25) %>% mutate(year=as.numeric(timestep)+1991) %>% 
  select(fire_year, species_test, density, year, abs.BA, species, subplot) %>% 
  pivot_wider(names_from = species, values_from = abs.BA) %>% 
  mutate_all(~replace(., is.na(.), 0))
  
colnames(res)[7:15] <- str_replace(colnames(res)[7:15], "_", " ")
#The PCA
res.pca <- PCA(res[7:15],  graph = FALSE)

#Graph of variances explained by the axes
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

cercle <- fviz_pca_var(res.pca,repel = TRUE) + theme(
  plot.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  text = element_text(size = 15),
  axis.text = element_text(size = 12))

cercle

# Get position of all subplot in the multivariate space
df_pca <- cbind(data.frame(get_pca_ind(res.pca)$coord), res)

#Centroids are recovered. 
centroid_fire_year <- df_pca %>% 
  group_by(year, fire_year, density, species_test) %>% 
  summarise(centroid_dim1=mean(Dim.1),
            centroid_dim2=mean(Dim.2))
  
#Remove harvested stands
centroid_fire_year <- centroid_fire_year %>% filter(!fire_year %in% c(1910, 1923))

centroid_fire_year$species_test <- factor(centroid_fire_year$species_test, 
                                               levels = c("control", "BOJ", "ERS", "ERR", "all"), 
                                               labels = c("Control", "Yellow Birch", "Sugar Maple", "Red Maple", "All"))

centroid_fire_year$density <- factor(centroid_fire_year$density, 
                                          levels = c("NA", "low", "medium", "high"), 
                                          labels = c("Control", "Low", "Medium", "High"))

#We add the control in the plots:
control <- filter(centroid_fire_year, density=="Control")
centroid_fire_year <- filter(centroid_fire_year, density!="Control", species_test!="All") %>% 
  bind_rows(mutate(control, species_test="Yellow Birch"),
            mutate(control, species_test="Red Maple"),
            mutate(control, species_test="Sugar Maple"))
  

colnames(centroid_fire_year)[2:3] <- c("Fire year", "Density")

p <- ggplot(centroid_fire_year, aes(x=centroid_dim1, y=centroid_dim2, 
                                    color=Density, fill=Density, 
                                    group=interaction(`Fire year`, Density), 
                                    shape=`Fire year`)) +
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_vline(xintercept=0, linetype="dashed") +
  geom_path(alpha=0.6) +
  geom_point(data=filter(centroid_fire_year, year==2100), aes(size=`Fire year`)) +
  scale_size_manual(values=c(2,2,2,2.5,2.5,2)) +
  geom_point(data=filter(centroid_fire_year, year==1991), aes(shape=`Fire year`), color="black", fill="black", size=4) +
  scale_shape_manual(values=c(8, 16, 15,18,17,25)) +
  facet_wrap(~species_test, nrow=2) +
  xlab("Dimension 1 (22.3%)") + ylab("Dimension 2 (17.1%)") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16))



pg <- ggplotGrob(p)
cercleg <- ggplotGrob(cercle)

pl <- gtable_filter(pg, 'panel', trim=F)$layout
pg <- gtable_add_grob(pg, cercleg, t=max(pl$t), l=max(pl$l))

grid.newpage()
grid.draw(pg) 

pdf(file = "../results/figures/PCA.pdf", width = 8.2, height = 8)
grid.newpage()
grid.draw(pg) 
dev.off()

# We change the position of the species name with inkscape software to make the graphic
# more visible
