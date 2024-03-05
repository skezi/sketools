barplot_maker = function(df,
                         var,
                         Pourcentage = F,
                         flip = T,
                         xlab_title = NULL,
                         ylab_title = NULL,
                         title = NULL,
                         palette_skezi = p_skezi){

  data = data_engine(df, var)

  if(Pourcentage == T){
    data[,3] = round(data[,3],2)
    names(data)[3] = "val"
  } else {
    names(data)[2] = "val"
  }

  ggplot(data, aes(x = group , y = val)) +

    geom_bar(stat = "identity", fill= palette_skezi[1], colour="black") +

    geom_text(aes(label = str_wrap(val,20),

                  position = "identity")) +

    theme(axis.text.x =  element_text (angle = 90, hjust = 1, vjust = 0.5 ))+

    {if(flip) coord_flip() } +

    xlab(xlab_title)+

    ylab(ylab_title) +

    ggtitle(title)

}
