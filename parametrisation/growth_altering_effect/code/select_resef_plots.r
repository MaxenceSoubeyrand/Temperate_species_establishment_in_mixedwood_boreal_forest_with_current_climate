# Script that opens the resef data and makes a pre-selection on the number of pairs 
# present in the data, so that some pairs are not over represented on others. 

rm(list=ls())

library(tidyverse)

# Preparing RESEF data -------------------------------------------

# Retrieve tables from the RESEF database
dendro <- readRDS(file="../data/dendro_RESEF.rds")
sp <- readRDS(file="../data/sp_RESEF.rds")
place <- readRDS(file="../data/place_RESEF.rds")

# Latitude of plots
place_lat <- select(place, PLACE, LAT = LATCALGPS)

# Calculation of X-Y coordinates of trees per plot
dendro <- mutate(dendro, PX = PEP %/% 10, PY = PEP %% 10,
                 X = round(PX*10 + AXEX/100, 2), 
                 Y = round(PY*10 + AXEY/100, 2))

# Selection of plots to be conserved (presence of one of the temperate tree species
# with one or more boreal species)
plots_sel <- c(102, 103, 105, 201, 403, 702, 1502)
plots_sel <- unique(dendro$PLACE)

# Selection and transformation of data for these plots
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
           ANNEE, DATE, ESS, Type, Diam, Height)

# Create a dataset for the neighbors of each tree
neighbors <- select(dendro_sel, PLS, PPA, X, Y, ESS, Diam)
# We join the database with itself (according to the same plot-measure PLS)
# to obtain the pairs of individuals, with the suffix "_n" to differentiate the columns  
neighbors <- inner_join(neighbors, neighbors, by = "PLS", suffix = c("", "_n"))
neighbors <- filter(neighbors, PPA != PPA_n) # remove the pairs from a tree and itself
neighbors <- mutate(neighbors, dist = sqrt((X - X_n)^2 + (Y - Y_n)^2))
# Keep the neighbors up to 10 m 
neighbors <- filter(neighbors, dist <= 10) %>%
    select(PLS, PPA, ESS, PPA_n, ESS_n, Diam_n, dist)

# Get the number of pairs
mat_pairs <- neighbors %>% 
    group_by(ESS, ESS_n) %>% 
    count() %>% 
    filter(ESS %in% c("BOJ","ERS", "ERR", "THO", "SAB", "EPB", "PET", "BOP")) %>% 
    filter(ESS_n %in% c("BOJ","ERS", "ERR", "THO", "SAB", "EPB", "PET", "BOP")) %>%  
    pivot_wider(names_from = ESS_n, values_from = n) 


mat_pairs

# For each couple in each inventory calculate the proportion of the representativity 
# of each specific pair
sel <- neighbors %>% 
    group_by(ESS, ESS_n, PLS) %>% 
    count() %>% 
    filter(ESS %in% c("BOJ","ERS", "ERR", "THO", "SAB", "EPB", "PET", "BOP", "PIG")) %>% 
    filter(ESS_n %in% c("BOJ","ERS", "ERR", "THO", "SAB", "EPB", "PET", "BOP", "PIG")) %>% 
    arrange(PLS) %>% 
    unite(couple, c(ESS, ESS_n), sep="_") %>% 
    mutate(PL=gsub('.{1}$', '', PLS),
           S=gsub('[0-9]+', '', PLS))%>% 
    group_by(PLS) %>% 
    mutate(prop_couple=n/sum(n))
    
# We retain all pairs with EPB because there are few of them
sel_EPB <- sel %>% 
    group_by(PLS) %>% 
    filter(any(str_detect(couple, "EPB")))

# We retain plots were less of 90% of specific pairs are sugar maple with sugar maple
# because these pair is over represented
sel_ERS <- sel %>% 
    group_by(PLS) %>% 
    filter(!any(couple=="ERS_ERS"&prop_couple>0.9))

plot <- unique(c(sel_ERS$PL, sel_EPB$PL))

# Vizualisation of the matrix of specific pairs:
sel_pairs <- filter(sel, PL %in% plot) %>% 
    group_by(couple) %>% 
    summarize(n=sum(n)) %>% 
    separate(couple, c('ESS', 'ESS_n'), sep="_") %>%  
    pivot_wider(names_from = ESS_n, values_from = n) 

sel_pairs

# We save the plot ID of plots retain
saveRDS(plot, "../results/selected_resef_plot.rds")
