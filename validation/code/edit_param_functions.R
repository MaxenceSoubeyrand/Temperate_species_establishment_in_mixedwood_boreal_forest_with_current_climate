# This function extracts the tree density data 
# from the "transect_meas.csv" file for a certain plot
# then formats it as an initial tree density list for the SORTIE parameter file

get_transect_init_dens <- function(data, plot, yr) {
    # This data frame is to translate between the species_id in the transect data
    #  and the species_name in the SORTIE model
    sp_tab <- data.frame(
        species_name = c("White_Cedar", "Balsam_Fir", "Mountain_Maple", "White_Spruce",
                         "Jack_Pine", "Trembling_Aspen", "Paper_Birch", "Sugar_Maple", 
                         "Red_Maple", "Yellow_Birch"),
        species_id = c("TOC", "ABA", "ASP", "PGL", "PBA", "PTR", "BPA", "ERS", "ERR", "BOJ")
    )
    
    dens_df <- data %>%
        # join with sp_tab to get species names
        inner_join(sp_tab, by = "species_id") %>%
        mutate(
            # change species_name to factor to get same ordering of species 
            #  as sORTIE file (not sure if that's important)
            species_name = factor(species_name, levels = sp_tab$species_name),
            # convert DBH class to size classes written as in SORTIE file (e.g.: 10 becomes s10.0)
            size_class = ifelse(dbh_class == 0.5, "Seedling", 
                                paste0("s", dbh_class, ".0")),
            # get density and convert density written as in SORTIE file
            density = paste0(density, ".0")
        ) %>%
        # sort by species and dbh_class to get same ordering as original parameter file
        #  (maybe not needed)
        arrange(species_name, dbh_class) %>% ungroup() %>%
        dplyr::select(species_name, size_class, density)
    
    # nest the data by species (so dens_nest has one row by species, 
    #  and the 2nd column contains one data frame by species)
    dens_nest <- nest_by(dens_df, species_name, .key = "size_density_data") %>%
        # retransform species name to character (not factor)
        mutate(species_name = as.character(species_name))
    
    # this creates the initial tree density list for one species (one row of dens_nest)
    create_init_dens_list <- function(species_name, size_density_data) {
        # for each row in size_density_data, produces a list of one element (the density)
        #  with one attribute (the size class)
        init_dens <- pmap(size_density_data,
                          function(size_class, density) structure(list(density), sizeClass = size_class))
        # set the name of every element of the list to "tr_initialDensity" to match SORTIE param file
        init_dens <- set_names(init_dens, rep("tr_initialDensity", length(init_dens)))
        # structure adds the whatSpecies attribute (species name) to the list
        structure(init_dens, whatSpecies = species_name)
    }
    
    # apply the function above to all species
    dens_nest_list <- pmap(dens_nest, create_init_dens_list)
    # name every element of list as "tr_idVals" to match SORTIE parameter file
    set_names(dens_nest_list, rep("tr_idVals", length(dens_nest_list)))
}

# This function create a SORTIE parameter file from the template "ferld_param_no_epi.xml"
# with the tree initial density from a given plot_id, and sets the number of timesteps
# and the output filenames to be saved in the given output directory.
create_param_file <- function(data, file_name, growth, timesteps, out_dir, par_dir, par_name) {
    # Read template parameter file from XML and convert to R list
    par_xml <- read_xml(par_name)
    par_list <- as_list(par_xml)
    
    # Produce new tree initial density info
    dens_new <- get_transect_init_dens(data=data) #plot=i; plot_id=i
    # Replace density info in original list
    par_list$paramFile$trees$tr_initialDensities <- dens_new
    
    # Change number of timesteps
    par_list$paramFile$plot$timesteps[[1]] <- as.character(timesteps)
    
    # Change output filenames
    #file_name_alnum <- str_replace_all(file_name, "[^[:alnum:]]", "")
    file_name_alnum <- file_name
    par_list$paramFile$Output$ou_filename[[1]] <- paste0(out_dir, "/", file_name_alnum, ".gz.tar")
    par_list$paramFile$ShortOutput$so_filename[[1]] <- paste0(out_dir, "/", file_name_alnum, ".out")
    
    #Change the potential growth of each species 
    n <- length(par_list$paramFile$NCIMasterGrowth7$gr_nciMaxPotentialGrowth) #n espèces
    for(i in 1:n){
        sp <- attributes(par_list$paramFile$NCIMasterGrowth7$gr_nciMaxPotentialGrowth[[i]])$species #La ième espèce
        growth_sp <- growth$growth[which(growth$species==sp)] #La croissance de la ième espèce dans le plot
        par_list$paramFile$NCIMasterGrowth7$gr_nciMaxPotentialGrowth[[i]][1] <- as.character(growth_sp) #On remplace la croissance dans le fichier de paramètres
    }

    # Convert back to XML format and save in new file
    par_xml_new <- as_xml_document(par_list)
    write_xml(par_xml_new, paste0(par_dir, "/", file_name_alnum, ".xml"))
}
