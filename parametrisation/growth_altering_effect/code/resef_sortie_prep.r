# In a first part, we modify the database of resef to combine it with 
# the ferld database and thus have more data for the parameterization. 
# In a second part, we create parameter files to launch them in SORTIE and obtain 
# the amount of light received by each tree. The quantity of light received by each 
# tree will be useful during the parametrization since it is the variable S. 
# The parameter files are executed with a batch file created here, the batch file
# execute SORTIE simulations for LDRTF and RESEF, so script to prepare data for
# LDRTF should execute before this script. 

#setwd à enlever après
setwd("~/PhD/Chap1/Github/parametrisation/growth_altering_effect/code")

library(tidyverse)
library(xml2)

# Preparing RESEF data -----

# Retrieve tables from the RESEF database
dendro <- readRDS(file="../data/dendro_RESEF.rds")
sp <- readRDS(file="../data/sp_RESEF.rds")
place <- readRDS(file="../data/place_RESEF.rds")

# Latitude of plots (for light calculation in SORTIE)
place_lat <- select(place, PLACE, LAT = LATCALGPS)

# Calculation of X-Y coordinates of trees per plot
dendro <- mutate(dendro, PX = PEP %/% 10, PY = PEP %% 10,
                 X = round(PX*10 + AXEX/100, 2), 
                 Y = round(PY*10 + AXEY/100, 2))

# Selection of plots to be conserved (presence of one of the temperate tree species
# with one or more boreal species)
plots_sel <- readRDS("../results/selected_resef_plot.rds")[-32]

# Transformation of data for these plots
# Warning: DHP in mm, Height in dm in RESEF data
# Note: DHP min of 10 cm and ETAT %in% c(10, 40) for live adults,  
#       SERIES indicates sequentially the different visits to the plots 
#       PPA is a unique identifier of the tree
dendro_sel <- dendro %>% 
    filter(PLACE %in% plots_sel, DHP >= 100,
           ETAT %in% c(10, 40)) %>%
    mutate(PLS = paste0(PLACE, SERIE), Type = "Adult",
           Diam = DHP / 10, Height = HTOT / 10) %>%
    inner_join(place_lat) %>%
    select(PLS, SERIE, PLACE, PPA, LAT, X, Y, 
           ANNEE, DATE, ESS, Type, Diam, Height) %>% arrange(PPA)

# We save the prepared data 
saveRDS(dendro_sel, "../results/dendro_sel_resef.rds")


# Prepare parameter files for light-----


# Correspondence between species codes and name in SORTIE
# Other species (mostly beech and ash) are coded as "Other".
# "Other" have the same allometric parameters as ERS in SORTIE
sp_names <- read.csv("../data/sp_names.csv")
# Preparation of tree maps for each combination of plot and series (PLS) 
# to calculate the light in SORTIE
# nest_by produces a data frame with PLS, LAT and  
# a "data" column containing the treemap data for this PLS
# (so each element of the column is itself a data.frame)
dendro_list <- dendro_sel %>%
    left_join(sp_names) %>%
    select(PLS, LAT, X, Y, Species, Type, Diam, Height) %>%
    replace_na(list(Species = "Other")) %>%
    nest_by(PLS, LAT)


# This function takes a model parameter file (calc_shade.xml) and produces parameter
# and treemap files for each PLS as well as .txt files for the treemaps

root_dir <- normalizePath(getwd())
root_dir <- str_replace(root_dir, "code", "results")
prep_sortie_files <- function(PLS, LAT, data) {
    write_tsv(data, paste0("../results/maps/treemap_", PLS, ".txt"))
    par_xml <- read_xml("../data/calc_shade.xml")
    par_list <- as_list(par_xml)
    par_list$paramFile$plot$plot_latitude[[1]] <- LAT
    par_list$paramFile$trees$tr_treemapFile[[1]] <- paste0(root_dir, "\\maps\\treemap_", PLS, ".txt") 
    par_list$paramFile$Output$ou_filename[[1]] <- paste0(root_dir, "\\shade_res\\shade_", PLS, ".gz.tar")
    par_list$paramFile$ShortOutput$so_filename[[1]] <- paste0(root_dir, "\\shade_res\\shade_", PLS, ".out")
    par_xml_new <- as_xml_document(par_list)
    write_xml(par_xml_new, paste0(root_dir, "\\maps\\calc_shade_", PLS, ".xml"))
}

# Executer la fonction pour chaque element de dendro_list
pwalk(dendro_list, prep_sortie_files)

# Once all the parameter files for the resef and LDRTF are created, 
# we create a batch file that will execute all the newly created parameter files. 
par_files <- dir("../results/maps", pattern = ".xml")
batch_xml <- read_xml("../data/calc_shade_all.xml")
batch_list <- as_list(batch_xml)
par_files_list <- map(
    par_files, 
    ~ list(ba_fileName = list(paste0(root_dir, "\\maps\\", .)),
           ba_numTimesToRun = list("1"))
)
names(par_files_list) <- rep("ba_parFile", length(par_files_list))
batch_list$batchFile <- par_files_list
write_xml(as_xml_document(batch_list), "calc_shade_all.xml")
#  Does not work as it is, you have to modify in the calc_shade_all.xml
# <batchFile> by <batchFile fileCode="06010401"> if it not already done
# Execute the batch file in sortie, Sortie will put the ouptut file in the shade_res directory. 