####################################################################################
################### crear lista de graficas ###################### 
####################################################################################

theme_th <- theme_void() +
  theme(
    plot.margin = margin(2,2,10,2,"pt"),
    plot.background = element_rect(fill = bck,
                                   color = NA),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5,
                                color = "white",
                                face = "bold",
                                size = 11
    ),
    
    legend.text = element_text(color = "white"
                               ),
    plot.subtitle = element_text(family = "Serif", 
                                 size = 14,
                                 face = 'italic',
                                 color = 'white'
    ),
    plot.caption = element_text(family = "Serif", 
                                size = 8,
                                color = 'white'
    ),
    plot.title = element_text(color = "white",
                              family = "Serif", 
                              size = 14,
                              face = 'italic') 
  )

# vector de valores de actividad economica ------------

AE_v <- shp$AE |> 
  unique()

# Proceso para crear lista de gráficas ------------------

cl_list <- list() # crear lista vacia

# función para incluir ploteos

ploteo <- function(data){
  
  for(i in AE_v){
    
    filter_data <- data[data$AE == i, ] # filtrar valores de AE
    
    # Función plots
    
    ploteo <- filter_data |> 
      ggplot(aes(fill = Qij)
                          ) +
      geom_sf() +
      labs(fill = "CL_Qij",
           title = unique(filter_data$AE),
           caption = 'Nota: Elaboración propia con datos del INEGI\n
      Censo Económico año 2004') +
      ggtitle(paste("FIGURA", (length(cl_list) + 1),
                    "\n",
                    unique(filter_data$AE)),
              ) +
      guides(
        fill = guide_legend(
          nrow = 1,
          title.position = "top",
          label.position = "bottom"
        )
      ) +
      scale_fill_manual(
        values = pal,
        # label = c("< 1","= 1","> 1 ","SinDato")
      ) +
      theme_th
      
      
    # Almacenar el gráfico en la lista
    
    cl_list[[paste(length(cl_list) +
                     1,sep = "_",
                   unique(filter_data$AE))]] <- ploteo
    
  }
  
  return(cl_list)
}
  
CLoc_Mich04 <- ploteo(shp)

CLoc_Mich04[[1]]

