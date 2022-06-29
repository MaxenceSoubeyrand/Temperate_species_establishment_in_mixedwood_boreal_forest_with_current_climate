# This script is used to extract the light data from the output simulations and 
# merge them with the data from LDRTF and REEF. We merge the data from LDRTF 
#and RESEF into one dataset. Finally, we create a dataset that for each tree, 
#lists all its neighbors, the distance to the focal tree, and their diameters.

rm(list=ls())

library(tidyverse)
library(xml2)

# Extract the shadow value calculated by SORTIE --------------------------

# Functions to extract results from SORTIE output
source("process_output_functions.R")

# Unzip the compressed folders and recover the file of interest
decompress_save_xml <- function(untar_folder){
  untar(untar_folder, exdir="shade_res")
  # Change for your directory
  path_folder2 <- "~/the/whole/directory/to/the/folder/where/the/results/of/SORTIE/ouptut/are/"
  path_tar2 <- paste0(path_folder2, substr(untar_folder,1,nchar(untar_folder)-7), "_1.xml.gz")
  gunzip(path_tar2)
  temp_xml <- read_xml(substr(path_tar2,1,nchar(path_tar2)-3))
  file_name <- substr(untar_folder, 11, nchar(untar_folder))
  file_name <- paste0(substr(file_name, 1, nchar(file_name)-7), "_1.xml")
  write_xml(temp_xml, paste0("shade_res/dezip/", file_name))
  unlink("shade_res/Users", recursive = TRUE)
}

untar_folder <- dir("shade_res", pattern = ".gz", full.names = TRUE)
map(untar_folder, decompress_save_xml)


# Combine all treemaps after one iteration into a single data.frame sortie_res
do_files <- dir("shade_res/dezip", pattern = "1_1.xml", full.names = TRUE)
do_PLS <- str_match(do_files, "/shade_(.*)_1_1.xml")[,2]
names(do_files) <- do_PLS

sortie_res <- map_dfr(do_files, ~ treemap_from_file(.)$adult, .id = "PLS")

# To match with the original dataset, n needs X, Y and the initial diameter obtained 
# by DBH - Growth/5 (because DBH is in cm and Growth is the radial growth in mm). 
# In the output file, Light is in fact the shading variable S (between 0 and 1, 
# larger value = more shading)
sortie_res <- sortie_res %>%
  mutate(Diam = round(DBH - Growth/5, 1)) %>%
  select(PLS, X, Y, Diam, Shading = Light)

# Combine with RESEF data and remove unwanted columns
dendro_resef <- readRDS("../data/dendro_sel_resef.rds")
dendro_resef <- select(dendro_resef, -LAT, -Type, -Height) %>% 
  mutate(PLACE=as.character(PLACE)) %>% 
  mutate(ESS=recode(ESS, `PIG`="Jack_Pine", `PET`="Trembling_Aspen",
                    `BOP`="Paper_Birch", `SAB`="Balsam_Fir",
                    `ERS`="Sugar_Maple", `EPB`="White_Spruce",
                    `BOJ`="Yellow_Birch", `ERR`="Red_Maple"))

# Combine with LDRTF data and remove unwanted columns
dendro_ldrtf <- readRDS("data/dendro_sel_ldrtf.rds") %>% 
  mutate(PLS=plot_id_year) %>% 
  separate(plot_id_year, c("PLACE", "ANNEE")) %>% 
  mutate(ANNEE=as.numeric(ANNEE)) %>% 
  rename(ESS=Species, PPA=stem_id) %>%
  select(-Type, -lat, -Height) 

# Combine the two data frame 
dendro <- bind_rows(dendro_resef, dendro_ldrtf)

# Adding the Light results
nci_dat <- left_join(dendro, sortie_res)

# Save the combine data
saveRDS(nci_dat, "../data/nci_dat.rds")
nci_dat <- readRDS("../data/nci_dat.rds")

# Create a dataset for the neighbors of each tree
neighbors <- select(nci_dat, PLS, PPA, X, Y, ESS, Diam)
# We join the database with itself (according to the same plot-measure PLS) to 
#obtain the pairs of individuals, with the suffix "_n" to differentiate the columns  
neighbors <- inner_join(neighbors, neighbors, by = "PLS", suffix = c("", "_n"))
neighbors <- filter(neighbors, PPA != PPA_n) # remove the pairs from a tree and itself
neighbors <- mutate(neighbors, dist = sqrt((X - X_n)^2 + (Y - Y_n)^2))
# Keep the neighbors up to 30 m in case we want them later
neighbors <- filter(neighbors, dist <= 30) %>%
  select(PLS, PPA, ESS, PPA_n, ESS_n, Diam_n, dist)

saveRDS(neighbors, "../data/neighbors.rds")
