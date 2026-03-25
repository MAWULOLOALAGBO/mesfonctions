analyser_na <- function(df){
  nb_miss_val <- colSums(is.na(df))
  prop_miss_val <- round(colSums(is.na(df))/nrow(df),2)
  tableau_recap <- data.frame(
    variables = names(df),
    nombre_na = nb_miss_val,
    proportion_na = prop_miss_val
  )
  tableau_recap_trie <- tableau_recap[order(tableau_recap$nombre_na),]
  tableau_recap_trie_tab <- tableau_recap[order(tableau_recap$nombre_na, decreasing = TRUE),]
  
  tableau_recap_trie$variables <- factor(tableau_recap_trie$variables,
                                        levels = tableau_recap_trie$variables) # pour permettre de garder l'ordre imposer pour le graphique
  
  plot_nb_na <- ggplot(tableau_recap_trie,
                    aes(x=nombre_na,
                        y=variables))+
    geom_segment(
      aes(x = 0, xend = nombre_na, y=variables, yend = variables),
      linewidth = 0.6,
      color = "black"
    )+
    geom_point(size =2,color = "black")+
    labs(title = "Repartition des valeurs manquantes par variables",
         x = "Nombre de valeur manquantes",
         y = NULL)+
    theme_minimal()
    
  list_elements <- list(tableau = tableau_recap_trie_tab,
                        graphique = plot_nb_na)
  return(list_elements)
}
