#Functions


exploratory_plot <- function(data.frame) {
  attach(data.frame)
scatterplot_output <- ggplot(data.frame,aes(y= species, x = age)) +
    geom_point(size = 4) +
    geom_smooth(method = "lm") +
    guides(color = guide_legend(title = "Site index")) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm")
          , axis.line = element_line(colour = "black", size = 1)
          , axis.ticks.length=unit(.25, "cm")
          ,  panel.background = element_blank()
          , axis.title.y = element_text(size = 22, face = "bold")
          , axis.text.y = element_text(size = 20)
          , axis.text.x = element_text(size = 20)
          , axis.title.x = element_text(size = 22, face = "bold" )
          , plot.title = element_text(size = 14)
          , legend.text = element_text(size = 20)
          , legend.title = element_text(size = 22, face = "bold")) +
    xlim(0,425) +
    ylim(0,30) 

return(scatterplot_output)
}

format_env_data <- function(lichenoldtrees_treedata, z) {
  lichenoldtrees_treedata |> 
    mutate(crownlength_percent = (crownlength/tree_height)*100) %>%
    mutate(site_index = as.character(site_index)) |>
    mutate(vegetation_type == factor(vegetation_type,levels = c("Myr",
                                                                "Sump",
                                                                "Lav",
                                                                "Blokkebeer",
                                                                "Berlyng", 
                                                                "Blaabeer", 
                                                                "Smaabregne",
                                                                "Hogstaude",
                                                                "laagurt"))) |>
    mutate(bark_structure.factor = factor(bark_structure, levels = c("1","2","3","4","5"))) %>%
    mutate(bark_color.factor = factor(bark_color, levels = c("1","2","3","4","5")))  |>
    mutate(Mork_Hengelav.factor = factor(Mork_Hengelav, levels = c("0","1","2"))) %>%
    mutate(Lys_Hengelav.factor = factor(Lys_hengelav, levels = c("0","1","2"))) %>%
    mutate(branch_thickness.bin = as.integer(branch_thickness > 0)) |>
    mutate(crookedness.factor = factor(crookedness, levels = c("1","2","3","4","5"))) -> lichenoldtrees_treedata
  #site_indexer blir numeriske 
  lichenoldtrees_treedata$site_index <- sub('.', '', lichenoldtrees_treedata$site_index)
  lichenoldtrees_treedata$site_index <- as.numeric(lichenoldtrees_treedata$site_index)
  lichenoldtrees_treedata$elevation <- as.numeric(lichenoldtrees_treedata$elevation )
  
  
  #Transforming branch_thickness into numeric
  lichenoldtrees_treedata$branch_thickness.numeric <- as.numeric(lichenoldtrees_treedata$branch_thickness)
  lichenoldtrees_treedata$vertical_forest_structure.factor <- as.factor(lichenoldtrees_treedata$vertical_forest_structure)
  factor(lichenoldtrees_treedata$vertical_forest_structure.factor, levels = c("Ensjiktet", "Tosjiktet", "Flersjiktet"))
  lichenoldtrees_treedata$plot <- factor(lichenoldtrees_treedata$plot)
  lichenoldtrees_treedata$age <- round(lichenoldtrees_treedata$age, digits = 0)
  
  
  lichenoldtrees_treedata |>
    mutate( surface_area_tree_m2 = ((((2*pi)*(((dbh^2)/2)))/((2*pi)*(dbh/2))*200))/10000) -> lichenoldtrees_treedata
  

  return(lichenoldtrees_treedata)
}


# functions for the ordination analyses

#formatting the data
# pine

pine_ordination_formatting <- function(pine.species_and_all_variables) {
  pine.species_and_all_variables %>% 
    dplyr :: select(tree_species:vertical_forest_structure.factor) -> variables.only.pine1
  
  
  pine.species_and_all_variables %>% 
    dplyr:: select(1:4) -> variables.only.pine2
  
  variables.only.pine <- cbind(variables.only.pine2, variables.only.pine1)
  
  species.only.pine <- pine.species_and_all_variables[,16:100] 
  
  species.only.pine %>%
    select(!row) %>% 
    replace(is.na(.), 0) -> species.only.pine
  
  
  
  colSums(species.only.pine == "0")
  empty_columns <- colSums(species.only.pine == "0") == nrow(species.only.pine)
  empty_columns
  
  species.only.pine <- species.only.pine[ ,!empty_columns]
  
  all_data <- cbind(species.only.pine,variables.only.pine )
  
  
  # how to find where the environmental variables start
  #grep("tree_species",colnames(pine.species_and_all_variables))
  
  
  ## remove the rows with two few species
  
  #
  all_data <- all_data[rowSums(all_data[,1:58])>2,]
  
  return(all_data)
}




# spruce
spruce_ordination_formatting <- function(variables) {
  spruce.species_and_all_variables %>% 
    dplyr :: select(100:149) -> variables.only.spruce1
  
  
  spruce.species_and_all_variables %>% 
    dplyr:: select(1:4) -> variables.only.spruce2
  
  variables.only.spruce <- cbind(variables.only.spruce2, variables.only.spruce1)
  
  species.only.spruce <- spruce.species_and_all_variables[,16:100] 
  
  species.only.spruce %>%
    select(!row) %>% 
    replace(is.na(.), 0) -> species.only.spruce
  
  
  
  colSums(species.only.spruce == "0")
  empty_columns <- colSums(species.only.spruce == "0") == nrow(species.only.spruce)
  empty_columns
  
  species.only.spruce<- species.only.spruce[ ,!empty_columns]
  
  all_data <- cbind(species.only.spruce,variables.only.spruce )
  
  
  # how to find where the environmental variables start
  grep("tree_number",colnames(all_data ))
  
  
  ## remove the rows with too few species
  
  #
  all_data <- all_data[rowSums(all_data[,1:65])>2,]
  
  return(all_data)
}


# checking the number of axes####
#x is the species matrix

nmds_axes <- function(x) {
k_vec <- 1:10
stress <- numeric(length(k_vec))
species.only.spruce_dij <- metaMDSdist(x, trace = FALSE)
set.seed(25)
for(i in seq_along(k_vec)) {
  sol <- metaMDSiter(species.only.spruce_dij, k = i,
                     trace = FALSE)
  stress[i] <- sol$stress
}
plot(k_vec, stress, type = "b", ylab = "Stress",
     xlab = "Dimensions")
}