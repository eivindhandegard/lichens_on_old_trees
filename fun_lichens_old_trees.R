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