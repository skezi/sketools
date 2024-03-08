
decision_maker2 = function(df,df_bbl,var){
  
  var_type_parts <- strsplit(df_bbl[df_bbl$Variable...Field.name == var, "Field.type" ], "_")[[1]]
  var_type <- var_type_parts[length(var_type_parts)]

  if (var_type == "radiogroup"){
    
    p = piechart_maker(df, var)
    
  }
  
  else if (var_type == "dropdown"){
    
    p = plot_theme(barplot_maker(df, var))
    
  }
  
  else if (var_type == "checkbox"){
    
    leng = length(na.omit(unique(unlist(strsplit(as.character(df[,var]), ", ")))))
    
    if (leng <= 4){
      
      p = diag_venn_maker(df, var)
      
    } else {
      
      p = plot_theme(upstPlot_maker(df, var))
      
    }
    
  }
  
  else if (var_type == "rating"){
    
    p = barplot_maker(df, var)
    
    return(p)
    
  }
  
  else if (var_type == "boolean"){
    
    p = piechart_maker(df, var)
    
  }
  
  else if (var_type == "text"){
    
    sous_type = df_bbl[df_bbl$Variable...Field.name == var, "Field.input.type" ]
    
    if (is.na(sous_type) || sous_type == "text"){
      
      wordcloud_maker(df, var)
      
    } else if (sous_type == "date") {
      
      
      data_vector = as.numeric(format(as.Date(df[,var]), "%Y"))
      
      p = compute_interval_labels(data_vector,
                                  num_intervals = 10,
                                  description = TRUE)
      
    } else if (sous_type == "number") {
      
      
      data_vector = df[,var]
      
      p = compute_interval_labels(data_vector,
                                  num_intervals = 5,
                                  description = TRUE)
      
    } else if (sous_type == "range") {
      
      p = plot_theme(histogramme_maker(df, var))
      
    } else if (sous_type == "time") {
      
      p = plot_theme(histogramme_maker(df, var))
      
    } else if (sous_type == "month") {
      
      p = plot_theme(barplot_maker(df, var))
      
    } else  {
      invisible()
    }
    
  }
  
  else if (var_type == "comment"){
    
    wordcloud_maker(df, var)
    
  }
  
  else if (var_type == "number"){
    
    
    if(!is.discrete(df[,var]))
    {
      p = histogramme_maker(df,var)
    }
    
    else
    {
      p = compute_interval_labels(df[,var], num_intervals = 6, description = TRUE)
    }
    
  }
  
  else if (is.numeric(df[,var])) {
    
    if(!is.discrete(df[,var], cutoff = 15)) # discret ou non
    {
      p = histogramme_maker(df,var)
    }
    
    else
    {
      invisible()
    }
    
  }
  
  else if (is.double(df[,var])){
    
    p = barplot_maker2(df,var)
    
  }
  
  return(p)
  
}