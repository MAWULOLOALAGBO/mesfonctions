box_binaire <- function(df_num_et_cible, var_cible_binaire){
  
  # variables numériques seulement (sauf la cible)
  vect_num <- names(df_num_et_cible)[names(df_num_et_cible) != var_cible_binaire]
  
  # liste de stockage
  boxp_list <- list()
  
  # Boucle sur les variables numériques
  for(j in vect_num){
    
    p <- ggplot(df_num_et_cible,
                aes(x = .data[[var_cible_binaire]], 
                    y = .data[[j]])) +
      geom_boxplot(fill = "red") +
      labs(title = paste("Dispersion de", j, "selon", var_cible_binaire),
           x = var_cible_binaire,
           y = j)
    
    boxp_list[[j]] <- p
  }
  
  return(boxp_list)
}

box_binaire(df_num_et_cible,"Outcome")
