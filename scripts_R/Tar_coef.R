# -------------- F_load_paquets R  ----------------
getwd()
source("F_load_paquetes.R")

# ----------- Data Base Access -------------

unique(odbc::odbcListDrivers()[[1]])

dbname <- "DATA/CensoEconómico_9-14.accdb"

con <- RODBC::odbcConnectAccess2007(dbname)
RODBC::sqlTables(con)

# -------------- Tabla Censo Económico ----------------

SECRE <- RODBC::sqlFetch(con, "SECRE_acc")

dfr <- SECRE[c('anio_censal','municipio', 'actividad_economica', 'H001A')] # Seleccion de columnas

# dfr <- dfr[!(is.na(dfr$municipio)), ]
# dfr <- dfr[!(is.null(dfr$municipio)), ]

# ----------------- Elaborar Matrix para 2004 ----------------------------

dfr_2004 <- dfr |> 
  dplyr::filter(anio_censal == 2004,
                municipio != "",
                actividad_economica != "Total Municipal") # filtro año elliminando totales

names(dfr_2004) <- c("Año", "Municipio", "AE", "POT") # ajuste de nombres

Mat_2004 <- dfr_2004 |> 
  tidyr::pivot_wider(names_from = 
                       c(Municipio),
                     values_from = POT,
                     values_fill = 0) # pasar municipios a columnas

Mat_2004 <- Mat_2004[,-1] # eliminar columna de año

# ---------------- Agregar totales por sector y región  ----------------------------
################### apply(dataframe, 1 = fila, 2 = columna, función) 

#  find the sum of each row and add newcolumn

df <- Mat_2004

df$Total_Sector <- df[-1] |> rowSums()

df$Total_Sector |> sum()


#  find the sum of each column and add newrow


tr <- df[-1] |> colSums()

df <- rbind(df, c('Total_región',tr))

# -------------- INDICADORES ESTÁTICOS  ----------------
#                Indicadores de estructura
# ---------------Participación del sector en la región

Part_sr <- sapply(Mat_2004[-1], function(x) (x/sum(x)*100))
Part_sr <- cbind(Mat_2004[1], Part_sr)

# sqlSave(con, Part_sr, tablename = "PartSR_2004", rownames = FALSE, append = TRUE)

# Participación de la región en el sector

Part_rs <- apply(Mat_2004[-1], 1, function(x) (x/sum(x)*100), simplify = TRUE)
Part_rs <- as.data.frame(Part_rs, row.names = NULL, optional = FALSE)

AE <- as.character(Mat_2004$AE)
names(Part_rs) <- Mat_2004$AE

Part_rs <- cbind(AE = rownames(Part_rs), Part_rs)
rownames(Part_rs) <- NULL

# -------------- Coeficiente de localización  ----------------

cor(Part_rs[-1])
cor(Part_sr[-1])

Part_sr
Part_rs_piv <- t(Part_rs)
colnames(Part_rs_piv) <- Part_rs_piv[1,]
Part_rs_piv <- cbind(AE = rownames(Part_rs_piv), Part_rs_piv)
rownames(Part_rs_piv) <- NULL
Part_rs_piv <- Part_rs_piv[-1,]


Part_rs_piv <- as.data.frame(Part_rs_piv)
#Part_rs_piv <- as.numeric(Part_rs_piv)

Part_rs_piv <- sapply(Part_rs_piv[-1], as.numeric)
Part_sr_piv <- as.matrix(Part_sr[-1])


CLoc <- (Part_sr_piv / Part_rs_piv) / 100
CLoc <- as.data.frame(CLoc)
CLoc$AE <- Mat_2004$AE
#CLoc <- cbind(AE = Part_rs$AE, CLoc)
CLoc


# -------------- Coeficiente de especialización  ----------------


Cesp <- (Part_sr_piv - Part_rs_piv) / 100
Cesp <- as.data.frame(Cesp)
Cesp$AE <- Mat_2004$AE
#Cesp <- cbind(AE = Part_rs$AE, Cesp)
Cesp

# ------------------------ shape_file ---------------------------------------

Mich_shp <- read_sf("DATA/Mich/conjunto de datos/16mun.shp") ## Michoacán

cl <- CLoc |> tidyr::pivot_longer(cols = -AE,
                      names_to = "NOM_MUN",
                      values_to = "CLoc")

ce <- Cesp |> tidyr::pivot_longer(cols = -AE,
                                  names_to = "NOM_MUN",
                                  values_to = "Cesp")


Mich <- left_join(Mich_shp, 
                  cl,
                  by = ("NOM_MUN"))



