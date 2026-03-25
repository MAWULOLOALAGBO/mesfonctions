his_box <- function(df_num){
  
  vect_num <- names(df_num)
  
  # listes vides pour stocker les plots
  histo_list <- list()
  boxp_list <- list()
  
  # Boucle histogrammes
  for(i in vect_num){
    p <- ggplot(df_num, aes(x = .data[[i]])) +
      geom_histogram(fill = "steelblue", aes(y = ..density..)) +
      geom_density(color = "red", linewidth = 0.8) +
      labs(title = paste("Répartition de", i),
           x = i)
    
    histo_list[[i]] <- p
  }
  
  # Boucle boxplots
  for(j in vect_num){
    p <- ggplot(df_num, aes(y = .data[[j]])) +
      geom_boxplot(fill = "darkblue") +
      labs(title = paste("Dispersion de", j),
           y = j)
    
    boxp_list[[j]] <- p
  }
  
  # Retourner une liste de listes
  return(list(histogrammes = histo_list,
              boxplots = boxp_list))
}
