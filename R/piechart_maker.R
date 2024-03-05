piechart_maker <- function(df, var,
                           pourcentage = TRUE,
                           palette_skezi = p_skezi,
                           xlab_title = NULL,
                           ylab_title = NULL,
                           title = NULL) {

  ## créer la table des données: ---------------------------------------------

  data <- data_engine(df, var)

  ## en pourcentage ou en eff?  ----------------------------------------------

  if (pourcentage == TRUE) {

    data[, 3] <- round(data[, 3], 2)
    names(data)[3] <- "val"

  } else {
    names(data)[2] <- "val"
  }

  ## plot le pie chart -------------------------------------------------------

  ggplot(data, aes(x = "",  y = val, fill = group)) +

    geom_col() +

    coord_polar(theta = "y") +

    geom_text(aes(label =  paste0(round(val, 1), "%")),

              position = position_stack(vjust = 0.5),

              color = "black",

              size = 1.8,

              fontface = "bold") +

    scale_fill_manual(values = palette_skezi) +

    coord_polar(theta = "y", start = 0) +

    xlab(xlab_title) +

    ylab(ylab_title) +

    ggtitle(title)

}
