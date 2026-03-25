his_box <- function(df_num){
  vect_num <- names(df_num)
  for(i in names(df_num)){
    graph <- ggplot(df_num,
                    aes(x=df_num[[i]]))+
      geom_histogram(fill = "steelblue",aes(y=..density..))+
      geom_density(color = "red",linewidth = 0.8)+
      labs(title = paste("repartition de ",i),
           x=i)
    print(graph)
  }
  return(graph)
}
