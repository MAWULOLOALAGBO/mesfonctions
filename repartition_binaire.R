plot_repartition_binaire <- function(data, var,
                                     labels = c("0" = "Non", "1" = "Oui"),
                                     titre = "Répartition",
                                     legende = "Variable") {
  
  # Convertir en symbole pour tidyverse
  var_sym <- rlang::sym(var)
  
  # Préparation des données
  rep <- data %>%
    count(!!var_sym) %>%
    mutate(prob = n / sum(n),
           prop = percent(prob))
  
  # Graphique
  p <- ggplot(rep,
              aes(x = prob, y = "",
                  fill = factor(!!var_sym))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = prop),
              position = position_stack(vjust = 0.5),
              color = "white") +
    coord_polar(theta = "x") +
    scale_fill_discrete(labels = labels) +
    labs(title = titre,
         fill = legende) +
    theme_void()
  
  return(p)
}

# utilisation
#plot_repartition_binaire(data_train,
#                         var = "Outcome",
#                         titre = "Répartition du diabète",
#                         legende = "Diabétique")
