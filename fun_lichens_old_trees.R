#Functions


exploratory_age_plot <- function(x) {
scatterplot_output <- ggplot(x,aes(y= species, x = age)) +
    geom_point(size = 5) +
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
