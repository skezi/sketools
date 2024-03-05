decision_maker2 <- function(df, df_bbl, var, palette = p_skezi) {

  ## je récupère le type de chaque variable
  var_type <- df_bbl[df_bbl$Variable...Field.name == var, "Field.type"]

  ## suivant le type j'appelle une fonction x
  if (var_type == "radiogroup") {

    p <- plot_theme(piechart_maker(df, var))

  }

  if (var_type == "dropdown") {
    p <- plot_theme(barplot_maker(df, var))

  }

  if (var_type == "checkbox") {

    leng <- length(na.omit(unique(unlist(strsplit(as.character(df[, var]), ", ")))))

    if (leng <= 3) {

      p <- plot_theme(diag_venn_maker(df, var))


    } else {
      p <- plot_theme(upstPlot_maker(df, var))

    }

  }

  if (var_type == "rating") {
    p <- plot_theme(barplot_maker(df, var))

  }

  if (var_type == "boolean") {
    p <- plot_theme(barplot_maker(df, var))

  }

  # if (var_type == "ranking"){
  #  \\p = plot_theme(listeordonnee_maker(df, var))
  #  \\print(p)
  #}

  if (var_type == "text") {

    if (class(df[, var]) == "integer") {

      p <- histogramme_maker(df, var)
    }

  }

  if (var_type == "comment") {
    p <- plot_theme(wordcloud_maker(df, var))

  }


  if (var_type == "number") {
    # vérifier que cest un chiffre (int, doub)


    if (!is.discrete(df[, var])) {
      p <- histogramme_maker(df, var)
    } else {
      p <- barplot_maker(df, var)
    }

  }

  return(p)

}
