pyramid_age_maker <- function(df, var,
                              axis_col = "black",                               
                              axis_size = 12,                                   
                              switch_col = FALSE,                             
                              title = "Pyramide d'âge"
                            ) {

  df$age <- 2024 - df[, var]

  df <- df %>%
    mutate(
      age_group = dplyr::case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age < 40 ~ "30-39",
        age >= 40 & age < 50 ~ "40-49",
        age >= 50 & age < 60 ~ "50-59",
        age >= 60 & age < 70 ~ "60-69",
        age >= 70 & age < 80 ~ "70-79",
        age >= 80             ~ "80 ou plus"
      ),
      
      age_group = factor(
        age_group,
        level = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80 ou plus")
      )
    )


  col_1 <- "#E5788F"
  col_2 <- "#79D2E6"


  data <- df %>%
    select(age_group,inf_sexe) %>%
    drop_na()


  sexe_data <- data %>%
    pull(inf_sexe) %>%
    fct_count() %>%
    arrange(desc(n)) %>%
    slice_head(n = 2) %>%
    pull(f)


  top_two_sexes <- data %>%
    count(inf_sexe) %>%
    arrange(desc(n)) %>%
    slice_head(n = 2)


  filtered_data <- df %>%
    filter(inf_sexe %in% top_two_sexes$inf_sexe)

  age_structure <- ggplot(data = data, aes(x = as.factor(age_group), fill = inf_sexe)) +
    geom_bar(data = subset(data, inf_sexe == sexe_data[1])) +
    geom_bar(data = subset(data, inf_sexe == sexe_data[2]),
             aes(y = after_stat(count) * (-1))) +


    ggtitle("Pyramide d'âge", paste0("n = ", nrow(filtered_data))) +

    xlab("Tranches d'âge") +
    ylab("Effectif") +
    labs(fill = "Sexe") +
    coord_flip() +
    theme(axis.text = element_text(size = axis_size, face = "bold",
                                   colour = axis_col),
          legend.key.size = unit(1, "cm"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"))


  if (switch_col == TRUE) {
    col_1 <- "#79D2E6"
    col_2 <- "#E5788F"
  }

  return(age_structure)

}
