library(ggplot2)
plot_theme <- function(g) {
  g <- g + theme(axis.text = element_text(size = 8, face = "bold", colour = "#A3DA8D"),
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "transparent"), #transparent panel bg
    plot.background = element_rect(fill = "transparent", color = NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_blank(), #transparent legend bg
    legend.box.background = element_blank() #transparent legend panel
  )
}
