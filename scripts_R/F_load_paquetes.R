# --- PREAMBULO ------

#AUMENTO DE LA MEMORIA DE JAVA
#options(java.parameters = "-Xmx9000m")
#options(guiToolkit = "RGtk2")
#FUNCION NOT IN 
`%notin%` = function(x,y) !(x %in% y)
#FUNCION NOT LIKE
`%notlike%` = function(x,y) !(x %like% y)

options(scipen = 999)

gc()

paq <- c("prettymapr", "ggspatial",
                "ggplot2", "readxl", "mapview", "sf",
                "viridis", "RColorBrewer", "raster", "terra",
                "tidyverse",
                "tmap", "data.table", "ggrepel", "leaflet",
                "scales")

paquetes <- c('RODBC', "ggplot2" , "sf", "odbc", "tidyverse")

load_paquetes <- function(paquetes) {
  missing_packages <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all required packages
  lapply(paquetes, library, character.only = TRUE)
}

load_paquetes(paquetes)