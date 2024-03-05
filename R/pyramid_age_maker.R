library(dplyr)
library(ggplot2)
library(forcats)
pyramid_age_maker <- function(df, var,
  axis_col = "black",                               #Axis' number's color
  axis_size = 12,                                   #Size of axis' legend
  #Color of the 2nd barplot (left)
  switch_col = FALSE,                               #Switch color of the 2 barplot
  title = "Pyramide d'âge"
) {

  df$age <- 2023 - df[, var]

  df <- df %>%
    mutate(
      # Create categories
      age_group = dplyr::case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age < 40 ~ "30-39",
        age >= 40 & age < 50 ~ "40-49",
        age >= 50 & age < 60 ~ "50-59",
        age >= 60 & age < 70 ~ "60-69",
        age >= 70 & age < 80 ~ "70-79",
        age >= 80             ~ "80 ou plus"
      ),
      # Convert to factor
      age_group = factor(
        age_group,
        level = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80 ou plus")
      )
    )




  #Palette
  col_1 <- "#E5788F"
  col_2 <- "#79D2E6"

  #Creation of data.frame with only "sex" & "age_group"
  data <- df %>%
    select(age_group,inf_sexe) %>%
    drop_na()

  #Data sexe to name modalities in the function and determine top two sexes
  sexe_data <- data %>%
    pull(inf_sexe) %>%
    fct_count() %>%
    arrange(desc(n)) %>%
    slice_head(n = 2) %>%
    pull(f)

  #To Determine the top two sexes for filtered_data
  top_two_sexes <- data %>%
    count(inf_sexe) %>%
    arrange(desc(n)) %>%
    slice_head(n = 2)

  #Filter data to include only the 2 sexes that are the most represented
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

  #Switch the color of the barplot if necessary
  if (switch_col == TRUE) {
    col_1 <- "#79D2E6"
    col_2 <- "#E5788F"
  }

  return(age_structure)

}
