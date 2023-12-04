# Shp - create column Qij --------------


shp <- Mich |>
  mutate(Qij = case_when(
    CLoc < 1 ~ "< 1", # Tampoco hay especialización en esta actividad
    CLoc == 1 ~ " 1 ", # No hay especialización en esta actividad
    CLoc > 1 ~ "> 1", # Especialización regional en esta actividad
    is.nan(CLoc) ~ "NA",  # Condición para NaN
  )
  )

# Set color palette --------------

pal <- c("#94d2bd","#005f73", "#001219")

# Set color background ------------

bck <- "#001219"

# Set theme ---------------------

theme_custom <- theme_void() +
  theme(
    plot.margin = margin(0,15,0,15,"pt"),
    plot.background = element_rect(fill = bck,
                                   color = NA),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5,
                                color = "white",
                                face = "bold"
                                ),
    legend.text = element_text(color = "white"),
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
                              face = 'italic'),
    axis.title = element_text(color = "white"),
    title = element_text(color = "white",
                         family = "Serif", 
                         size = 14,
                         face = 'italic'),
    strip.text = element_text(color = "white",
                              family = "Serif", 
                              size = 9,
                              face = 'italic',
                              angle = 0,
                              hjust = 0
                              )
    
  )

# ggplot Make choropleth -------------------------

ggplot(shp,
       aes(fill = Qij)
       ) +
  geom_sf() +
  labs(fill = "Coeficiente de Localización",
      title = 'Figura',
      # x = 'Municipio',
      # y = 'Porcentaje del año 2020',
      caption = 'Nota: Elaboración propia con datos del INEGI\n
      Censo Económico año 2004\n Población Total Ocupada'
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
  theme_custom +
  facet_wrap(~ AE, ncol = 4)

