#Script for comparing validation data and simulation for adult tree 
#Basal area comparison and quantile comparison

rm(list=ls())

library(tidyverse)
theme_set(theme_bw())


data_val <- readRDS("../data/PEP_validation_classe.rds") #Validation data

#Find the year instead the simulation time
data_year_min <- data_val %>% group_by(plot_id) %>% summarise(year_min=min(year))
data_year_min$plot_id <- as.character(data_year_min$plot_id)
data_year_min$year_min<- as.numeric(data_year_min$year_min)

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

data_val$year <- as.numeric(data_val$year)

#Convert data validation in values comparable with the SORTIE outputs.
data_val <- data_val %>% mutate(stage=ifelse(dbh_class>10, "Adult", "Sapl")) %>%
  mutate(dbh_class=dbh_class-2.5) %>% #pour prendre la moyenne entre la classe sup et la classe inf. 
  group_by(plot_id, year, stage) %>% mutate(dens_plot=sum(density), BA_plot=sum(pi*((dbh_class/200)^2)*density)) %>%
  group_by(plot_id, year, species, stage) %>% 
  summarize(Abs.Den = sum(density),
         Abs.BA = sum(pi*((dbh_class/200)^2)*density), #Pour passer de cm2 ? m2 par hectare
         Rel.Den = sum(density)/mean(dens_plot), 
         Rel.BA = sum(pi*((dbh_class/200)^2)*density)/mean(BA_plot)) %>%
  mutate(plot_id=as.character(plot_id),
         species=as.factor(species),
         Stage=as.factor(stage),
         type=rep("Validation", n()))

data_val_all <- data_val %>% 
  group_by(plot_id, year, species, type) %>% 
  summarise(Abs.Den=sum(Abs.Den, na.rm=T), Abs.BA=sum(Abs.BA, na.rm=T),
            Rel.Den=sum(Rel.Den, na.rm=T), Rel.BA=sum(Rel.BA, na.rm=T))
data_val_all$stage <- "All"

data_val <- rbind(data_val, data_val_all)


data_val_adult <- data_val %>% filter(stage=="Adult")

#Find the year instead the simulation time
data_year_min <- data_val_adult%>% group_by(plot_id) %>% summarise(year_min=min(year))
data_sim <- readRDS("../results/res_detailed.rds") #Results of the simulation 

data_sim <- data_sim %>% 
  left_join(data_year_min) %>% 
  mutate(timestep=as.numeric(timestep), 
         year=timestep+year_min) %>%
  dplyr::select(-year_min) %>% na.omit()

data_sim_sum <- data_sim %>% 
  group_by(species, year, subplot, plot_id) %>% 
  summarize(ST_HA=sum((DBH/2/100)^2*pi*25)) %>% 
  group_by(species, year, plot_id) %>% 
  summarise(mean_ST=mean(ST_HA),
            n=n(),
            sd_ST=sd(ST_HA),
            qt_min=quantile(ST_HA, 0.05),
            qt_max=quantile(ST_HA, 0.95))

data_sim_sum$species <- as.factor(data_sim_sum$species)
data_sim_sum$species <- factor(data_sim_sum$species, 
                                          levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                                     "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                                     "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                                          labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                                     "Red Maple", "Sugar Maple", "Trembling Aspen",
                                                     "White Cedar", "White Spruce", "Yellow Birch"))

data_val_adult$species <- as.factor(data_val_adult$species)
data_val_adult$species <- factor(data_val_adult$species, 
                                     levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                                "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                                "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                                     labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                                "Red Maple", "Sugar Maple", "Trembling Aspen",
                                                "White Cedar", "White Spruce", "Yellow Birch"))

data_sim_sum <- filter(data_sim_sum, !plot_id%in%c("8809410701", "8809411502")) %>%
  group_by(plot_id) %>% mutate(plot_name=as.factor(cur_group_id()), type="Simualtion")

data_val_adult <- filter(data_val_adult, !plot_id%in%c("8809410701", "8809411502")) %>%
  group_by(plot_id) %>% mutate(plot_name=as.factor(cur_group_id()), mean_ST=Abs.BA)

val_BA <- bind_rows(data_sim_sum, data_val_adult)

# With quantile at 0.9
png(filename = "../results/figures/val_adult_BA.png", width = 1000, height = 700)
ggplot()  + 
  geom_ribbon(data=data_sim_sum, aes(y=mean_ST, x=year, ymin=qt_min, ymax=qt_max, fill=species), alpha=0.1) +
  facet_wrap(~plot_name, scales="free") +
  geom_line(data=val_BA, aes(y=mean_ST, x=year, color=species, linetype=type), size=1) +
  ylab(expression ("Basal Area "~(m^2/h))) + ylim(0, NA) +
  xlab("Year") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12)) +
  scale_fill_discrete(name = "Species") +
  scale_color_discrete(name = "Species") +
  scale_linetype_discrete(name = "")
dev.off()

##Rank validation
data_sim_rank <- data_sim %>% 
  group_by(species, year, subplot, plot_id) %>% 
  summarize(ST_HA=sum((DBH/2/100)^2*pi*25)) 

data_sim_rank$species <- as.factor(data_sim_rank$species)
data_sim_rank$species <- factor(data_sim_rank$species, 
                                     levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                                "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                                "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                                     labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                                "Red Maple", "Sugar Maple", "Trembling Aspen",
                                                "White Cedar", "White Spruce", "Yellow Birch"))

adult_ba_val <- mutate(data_val_adult, subplot = 0) %>%  # subplot = 0 for the observations
  select(species, year, subplot, plot_id, ST_HA = Abs.BA)
adult_ba_sim <- ungroup(data_sim_rank) %>%
  # only keep combinations species / year / plot_id present in the observations
  semi_join(adult_ba_val, by = c("species", "year", "plot_id")) 

# combine the two data sets
adult_ba_val <- bind_rows(adult_ba_val, adult_ba_sim)

# determine the growth in land area in each interval
adult_ba_val <- adult_ba_val %>%
  group_by(species, plot_id, subplot) %>%
  arrange(species, plot_id, subplot, year) %>%
  mutate(prev_yr = lag(year))

adult_ba_val2 <- select(adult_ba_val, species, plot_id, subplot, 
                        prev_yr = year, prev_ST = ST_HA)
adult_ba_diff <- inner_join(adult_ba_val, adult_ba_val2) %>%
  mutate(ST_growth = (ST_HA - prev_ST) / (year - prev_yr))

# calculate the rank of values in each species/plot_id/year combination
# and the corresponding quantiles
adult_ba_diff <- group_by(adult_ba_diff, species, plot_id, year) %>%
  mutate(rank = rank(ST_growth), quantile = (rank - 1/2) / max(rank)) %>% 
  filter(!species %in% c("Trembling Aspen", "Jack Pine")) %>% 
  group_by(plot_id) %>% mutate(plot_name=as.factor(cur_group_id()))

# display the quantiles for the observations (subplot = 0), a bit like a p-value
png(filename = "../results/figures/quantile_BA.png", width = 800, height = 650)
ggplot(filter(adult_ba_diff, subplot == 0), 
       aes(x = year, y = quantile, color = plot_name)) +
  geom_point() +
  geom_line() +
  facet_wrap(~species) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  scale_color_discrete("Plot ID") + 
  ylab("Quantile") +
  xlab("Year")
dev.off()



## Adult growth validation----
##growth comparison 
chemin_data_base <- "../data/PEP.mdb"
PEP <- odbcConnectAccess2007(chemin_data_base)

data_sim <- readRDS("../results/res_detailed.rds") #Results of the simulation 

placette <- unique(data_sim$plot_id)
arbre <- sqlFetch(PEP, "DENDRO_ARBRES") 

#remove the inventory with spruce budworm outbreak
data_val <- arbre %>% filter(ID_PE %in% placette) %>% 
  dplyr::select(ID_PE, ID_ARBRE, ID_PE_MES, ESSENCE, DHP) %>% 
  mutate(ID_ARBRE=as.factor(ID_ARBRE)) %>% 
  na.omit()

placette_MES <- sqlFetch(PEP, "PLACETTE_MES") %>% 
  filter(ID_PE %in% placette) %>% 
  dplyr::select(ID_PE, ID_PE_MES, DATE_SOND) %>% 
  mutate(DATE_SOND=substr(DATE_SOND,1,4))

data_growth <- data_val %>% left_join(placette_MES) %>%
  mutate(DATE_SOND=as.numeric(DATE_SOND)) %>% 
  filter(DHP>=100) 

epi <- read.csv("../data/pep_epidemies.csv") %>% 
  group_by(ID_PE) %>%  
  summarize(Annee_epi=max(Annee))

data_growth_epi <- left_join(data_growth, epi)
data_growth_epi$Annee_epi[is.na(data_growth_epi$Annee_epi)] <- 0
data_growth_epi <- data_growth_epi %>% filter(DATE_SOND>Annee_epi) 

data_growth <- NULL
for(i in unique(data_growth_epi$ID_PE)){
  d <- filter(data_growth_epi, ID_PE==i)
  if(length(unique(d$DATE_SOND))>1){
    data_growth <- rbind(data_growth, d)
  }
}

data_growth$ESSENCE <- dplyr::recode(data_growth$ESSENCE,
                             SAB="Balsam_Fir",
                             BOP="Paper_Birch",
                             EPB="White_Spruce",
                             ERR="Red_Maple",
                             ERS="Sugar_Maple",
                             BOJ="Yellow_Birch",
                             PIG="Jack_Pine",
                             THO="White_Cedar",
                             PET="Trembling_Aspen")

data_growth <- data_growth %>% 
  group_by(ID_ARBRE, ID_PE) %>% 
  mutate(growth_sondage=DHP-lag(DHP),
         temps_growth=DATE_SOND - lag(DATE_SOND),
         growth=growth_sondage/(temps_growth*10)) %>% 
  ungroup() %>% rename(plot=ID_PE) %>% 
  na.omit() %>%  
  dplyr::select(plot, ESSENCE, growth) %>% 
  rename(species=ESSENCE, growth=growth) %>% 
  mutate(type="observation")

data_sim <- data_sim %>% 
  filter(timestep!=0) %>% 
  mutate(Growth=Growth/5) %>% #growth on 5 years
  dplyr::select(plot=plot_id, species, Growth) %>% 
  rename(growth=Growth)%>% 
  mutate(type="simulation") %>% 
  na.omit()

res <- rbind(data_growth, data_sim)

res_qt <- res %>% 
  group_by(plot, species, type) %>% 
  summarize(n=n(),
            mean_growth=mean(growth),
            sd_growth=sd(growth),
            qt_min=quantile(growth,0.05),
            qt_max=quantile(growth,0.95))

res_qt$species <- factor(res_qt$species, 
                                     levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                                "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                                "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                                     labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                                "Red Maple", "Sugar Maple", "Trembling Aspen",
                                                "White Cedar", "White Spruce", "Yellow Birch"))

res_qt[res_qt$qt_min<0, "qt_min"] <- 0

res_qt <- filter(res_qt, !plot%in%c("8809410701", "8809411502")) %>%
  group_by(plot) %>% mutate(plot_name=as.factor(cur_group_id()))

# With quantile at 0.9
png(filename = "../results/figures/val_adult_growth_by_species.png", width = 900, height = 900)
ggplot(res_qt, aes(x=plot_name, y=mean_growth, ymin=qt_min, ymax=qt_max, color=type))  + 
  facet_wrap(~species, scales="free_x",ncol=3) +
  
  geom_errorbar(width=0.4, position = position_dodge(width=0.5), size=0.8) +
  ylab("Growth (cm)") + xlab("Plot ID") + ylim(0,NA) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12))+
  scale_color_discrete(name = "")
dev.off()

## Rank growth validation

data_growth <- mutate(data_growth, subplot = 0, plot_id=as.factor(plot)) %>%   # subplot = 0 for the observations
  select(-type, -plot)

data_sim <- readRDS("../results/res_detailed.rds") #Results of the simulation 

adult_growth <- data_sim %>% 
  mutate(growth=Growth/5) %>% 
  select(plot_id, species, growth, subplot)

# combine the two data sets
growth <- bind_rows(data_growth, adult_growth)

growth_diff <- na.omit(growth) %>% 
  filter(growth>0) %>% 
  group_by(species, plot_id) %>%
  mutate(rank = rank(growth), 
         quantile = (rank) / max(rank)) %>% 
  arrange(plot_id, species, growth)

growth_diff_0 <- filter(growth_diff, subplot == 0) %>% 
  group_by(plot_id) %>% mutate(plot_name=as.factor(cur_group_id())) %>% 
  filter(species !="Jack_Pine") %>% 
  mutate(species=str_replace(species, "_", " "))

png(filename = "../results/figures/quantile_growth.png", width = 800, height = 650)
ggplot(growth_diff_0, aes(x = plot_name, y = quantile)) +
  geom_jitter(width=0.2) +
  stat_summary(fun=mean ,color="red") +
  facet_wrap(~species, scale="free_x") +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  xlab("Plot ID") + ylab("Quantile")
dev.off()
