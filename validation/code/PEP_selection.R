# Script to load permanent plot data of the Quebec government, select 10 plots for 
# the validation process and transform the data to be used by SORTIE

rm(list=ls())

library(tidyverse)

#####TREES#####
# Opening data:  
tree <- readRDS(file="../data/PEP_tree.rds")

station <- readRDS("../data/station_PEP.rds")

placette_MES <- readRDS("../data/station_PEP.rds")

# We merge the table to have the DHP and the years.
tree_MES <- inner_join(tree, placette_MES, by="ID_PE_MES")

# Plots in the balsam-fir paper and yellow birch
tree_SBJ <- inner_join(tree_MES, station, by="ID_PE_MES") %>%
  filter(str_detect(GUIDE_ECO, "[45]")) %>%
  filter(str_detect(GUIDE_ECO, "[abcd]")) %>% 
  filter(is.na(PERTURB)) %>%
  select(-PERTURB)

tree_SBJ_class <- tree_SBJ  %>% mutate(dbh_class=round(DHP/50)*50/10) %>% # Rounded to the highest 5mm class. 
  group_by(ID_PE, DATE_SOND, ESSENCE, dbh_class) %>% summarize(count=n()) %>% 
  mutate(stage="adult",
         status_id = "A") %>% 
  rename(plot_id=ID_PE, year=DATE_SOND, species_id=ESSENCE) %>% 
  relocate(status_id, .after = species_id)

####Juveniles####
sapling <- sqlFetch(PEP, "DENDRO_GAULES") %>%
  dplyr::select(ID_PE, ID_PE_MES, ESSENCE, CL_DHP, NB_TIGE)

#On merge le tableau pour avoir le DHP et les années.
sapling_MES <- inner_join(sapling, placette_MES, by="ID_PE_MES")

sapling_SBJ <- inner_join(sapling_MES, station, by="ID_PE_MES") %>%
  filter(str_detect(GUIDE_ECO, "[45]")) %>%
  filter(str_detect(GUIDE_ECO, "[abcd]")) %>% 
  filter(is.na(PERTURB)) %>%
  select(-PERTURB)

sapling_SBJ$status_id <- rep("A", nrow(sapling_SBJ))

sapling_SBJ <- sapling_SBJ %>% dplyr::select(ID_PE, DATE_SOND, ESSENCE, status_id, CL_DHP, NB_TIGE) %>%
  mutate(CL_DHP=ceiling(CL_DHP/5)*5) %>% #Arrondi à la classe de 5cm la plus élevé.
  group_by(ID_PE, DATE_SOND, status_id, ESSENCE, CL_DHP) %>% 
  summarize(count=sum(NB_TIGE)) %>% 
  mutate(stage="sapling") %>% 
  rename(plot_id=ID_PE, year=DATE_SOND, species_id=ESSENCE, dbh_class=CL_DHP)


tree_sapling <- rbind(tree_SBJ_class, sapling_SBJ)

# Remove plot_id with only one invetory
d <- unique(tree_sapling[c("plot_id","year")])
d <- d %>% group_by(plot_id) %>% summarise(n=n())
plot_id_unique <- d[d$n==1, "plot_id"]

tree_sapling <- subset(tree_sapling, !tree_sapling$plot_id %in% plot_id_unique$plot_id)
tree_sapling <- tree_sapling %>% arrange(plot_id, year, species_id)



# Remove the species we don't model
esp <- c("BOP", "BOJ", "EPB", "ERR", "ERS", "PET", "PIG", "SAB", "THO", "ERR", "BOJ")

data <- tree_sapling
dataSTallesp <- data %>% mutate(density=dbh_class*count) %>% 
  group_by(plot_id, year) %>% summarise(ST=sum(density)) #Basal area of all species present in plots by year

dataSTesp <- data %>% mutate(density=dbh_class*count) %>% 
  group_by(plot_id, year) %>% filter(species_id %in% esp) %>% summarise(ST2=sum(density)) #Basal area of species of interest in the plots by year.


# Remove plots with anything other than target species
plot_esp <- left_join(dataSTallesp, dataSTesp, by = c("plot_id" , "year")) %>% 
  mutate(diff=1-ST2/ST) %>% group_by(plot_id)%>% summarise(diff=max(diff)) %>% 
  filter(diff==0) %>% dplyr::select(plot_id)

dataesp <- filter(data, plot_id %in% plot_esp$plot_id)

# Read a files that inform if plots where affected by spruce budworm outbreak
# in order to remove them
epi <- read.csv(file="../data/pep_epidemies.csv") %>% 
  group_by(ID_PE) %>%  
  summarize(Annee=max(Annee)) %>% 
  rename(plot_id=ID_PE)

data_epi <- left_join(dataesp, epi)
data_epi$Annee[is.na(data_epi$Annee)] <- 0
data_epi <- data_epi %>% filter(year>Annee) 
ID_apres_epi <- unique(data_epi[,1:2])[,1][duplicated(unique(data_epi[,1:2])[,1]),]
data_epi <- data_epi %>%  filter(plot_id %in% ID_apres_epi$plot_id)


# We will keep about 12 plots with as many inventories as possible and as many species as possible. 
data_epi <- data_epi %>% mutate(plot_size = case_when((stage == "sapling") ~ 250,
                                              stage== "adult" ~ 25),
                        density = count * plot_size) %>%
  dplyr::select(-plot_size, -count) %>% ungroup()

data_year_min <- data_epi %>% group_by(plot_id) %>% summarise(year_min=min(year))
data_year_min$plot_id <- as.character(data_year_min$plot_id)
data_year_min$year_min<- as.numeric(data_year_min$year_min)


#Change species names in data validation
colnames(data_epi)[2:3] <- c("Year", "Species")
data_epi$Species[data_epi$Species=="BOJ"] <- "Yellow_Birch"
data_epi$Species[data_epi$Species=="BOP"] <- "Paper_Birch"
data_epi$Species[data_epi$Species=="ERS"] <- "Sugar_Maple"
data_epi$Species[data_epi$Species=="ERR"] <- "Red_Maple"
data_epi$Species[data_epi$Species=="THO"] <- "White_Cedar"
data_epi$Species[data_epi$Species=="PET"] <- "Trembling_Aspen"
data_epi$Species[data_epi$Species=="PIG"] <- "Jack_Pine"
data_epi$Species[data_epi$Species=="SAB"] <- "Balsam_Fir"
data_epi$Species[data_epi$Species=="EPB"] <- "White_Spruce"

data_epi$Year <- as.numeric(data_epi$Year)

#Convert data validation in values comparable with the SORTIE outputs.
data_val <- data_epi %>%
  mutate(dbh_class=dbh_class-2.5) %>% #pour prendre la moyenne entre la classe sup et la classe inf. 
  group_by(plot_id, Year, stage) %>%
  mutate(dens_plot=sum(density),
         BA_plot=sum(pi*((dbh_class/2)^2)*density)/10000) %>%
  group_by(plot_id, Year, Species, stage) %>% 
  summarize(Abs.Den = sum(density),
            Abs.BA = sum(pi*((dbh_class/2)^2)*density)/10000, #Pour passer de cm2 ? m2 par hectare
            Rel.Den = sum(density)/mean(dens_plot), 
            Rel.BA = sum(pi*((dbh_class/2)^2)*density)/10000/mean(BA_plot)) %>%
  mutate(plot_id=as.character(plot_id),
         Species=as.factor(Species),
         stage=as.factor(stage)) %>%
  mutate(Type="Validation") %>% 
  filter(stage=="adult")

ggplot(data_val, aes(x=Year, y= Abs.BA, color=Species)) + geom_line(size=1.3) + facet_wrap(~plot_id)

# Based on these plots, we selected 12 plots with as much inventory as possible 
# and the coexistence of temperate tree species and mixed boreal forest species

# Plots selected 
site_sel_visu <- c("7109503802", "7609507102", "7609509002", "7601505301", "7608804302", "8809411502", "8809410701",
                   "7209904001", "7209606102", "1219611102", "7100813401", "7209406201")

data_sel_visu <- data_val %>% filter(plot_id %in% site_sel_visu)

# Two plots were selected with pure jack pine, these two plots have been removed
# because in the validation process we used PCA, and PCA works in community with 
# at least two species in the plot

ggplot(data_sel_visu, aes(x=Year, y= Abs.BA, color=Species)) + geom_line(size=1.3) + facet_wrap(~plot_id)


data <- dataesp %>% filter(plot_id %in% unique(data_sel_visu$plot_id)) %>% 
  mutate(plot_size = case_when(stage=="sapling" ~ 250,
                               stage=="adult" ~ 25),
         density = count * plot_size) %>%
  dplyr::select(-plot_size, -count, -stage) %>% 
  group_by(plot_id, year, species_id, dbh_class) %>%
  summarise(density=sum(density))

saveRDS(data, file="../data/PEP_validation_classe.rds") #save the data to use it as initial condition and validation data