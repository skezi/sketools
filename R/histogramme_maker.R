histogramme_maker <- function(data, var,
                              x_lab = NULL,
                              y_lab = NULL,
                              clr = "#000000",
                              clr_fill = p_skezi[1],
                              palette = p_skezi,
                              ltype = "solid",
                              density_test,
                              mean_test,
                              title_lab = NULL) {

  ggplot(data, aes_string(x= var)) +

    geom_histogram(color=clr, fill = clr_fill,
                   linetype = ltype, alpha = 0.5, position = "stack", stat = "count") +
    xlab(x_lab) +

    ylab (y_lab) +

    ggtitle(paste(title_lab, "effectif", length(na.omit(data[, var])))) +

    geom_density(color = palette[1]) +

    theme(axis.text = element_text(size = 8, face = "bold", color = palette[1]),
          legend.key.size = unit(1, "cm"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "black", size = 0.2, linetype = "dashed"),
          panel.grid.minor = element_line(color = "black", size = 0.2, linetype = "dashed"),
          legend.background = element_blank(),
          legend.box.background = element_blank())
}
