# ------------------------------------------------------------------------------
# Anexo 1: Gráficos de la situación y evolución de la pandemia de Covid-19 en 
#           España
# ------------------------------------------------------------------------------

# Carga de las librerías
library(easypackages)
paq <-  c("readxl", "sqldf", "ggplot2"); libraries (paq)


# Importación de la Base de Datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/4. Gráficos/Bases de datos")

Base_diag <- read_excel("casos_diag_ccaadecl.xlsx")


# Gráfico 1: Serie de diagnósticos diarios
diag <- sqldf('
               SELECT fecha, SUM(Total) AS Diag_diarios
               FROM Base_diag
               GROUP BY fecha
              ')

png(filename = "Diag_diarios.png", width = 550, height = 375)

plot(diag,main="Gráfico 1. Diagnósticos diarios",ylab="nº diagnosticos", type= "l") + grid(nx=29)

dev.off()

# Gráfico 2: Serie de defunciones diarias
# Debido a la gran dimensión de la base de datos original, se han tenido que 
# partir y procesar en dimensiones más pequeñas.

   # -----
   # 2020
   # -----

   # Enero - Junio
   def_16_2020 <- read.table("casos_hosp_uci_def_sexo_edad_provres.txt", 
                             header = TRUE, sep = ",", nrows= 300000)

   def_dia_16_2020 <- sqldf('
      SELECT fecha, SUM(num_casos) AS CASOS_DIA, SUM(num_def) AS DEF_dia
      FROM def_16_2020
      GROUP BY fecha
      HAVING fecha < "2020-07-01"
   ')
   
   # Julio - Diciembre
   def_712_2020 <- read.table("casos_hosp_uci_def_sexo_edad_provres.txt", 
                              header = TRUE, sep = ",", nrows= 320000, 
                              skip=280000)
   
   colnames(def_712_2020) <- colnames(def_16_2020)
   
   def_dia_712_2020 <- sqldf('
      SELECT fecha, SUM(num_casos) AS CASOS_DIA, SUM(num_def) AS DEF_dia
      FROM def_712_2020
      GROUP BY fecha
      HAVING fecha > "2020-07-31" AND fecha < "2021-01-01"
   ')

   # -----
   # 2021
   # -----
   
   # Enero - Junio
   def_16_2021 <- read.table("casos_hosp_uci_def_sexo_edad_provres.txt", 
                             header = TRUE, sep = ",", nrows= 300000, 
                             skip=581000)
   
   colnames(def_16_2021) <- colnames(def_16_2020)
   
   def_dia_16_2021 <- sqldf('
      SELECT fecha, SUM(num_casos) AS CASOS_DIA, SUM(num_def) AS DEF_dia
      FROM def_16_2021
      GROUP BY fecha
      HAVING fecha > "2020-12-31" AND fecha < "2021-07-01"
   ')
   
   # Julio - Diciembre
   def_712_2021 <- read.table("casos_hosp_uci_def_sexo_edad_provres.txt", 
                              header = TRUE, sep = ",", nrows= 300000, 
                              skip=869000)
   
   colnames(def_712_2021) <- colnames(def_16_2020)
   
   def_dia_712_2021 <- sqldf('
      SELECT fecha, SUM(num_casos) AS CASOS_DIA, SUM(num_def) AS DEF_dia
      FROM def_712_2021
      GROUP BY fecha
      HAVING fecha > "2021-06-31" AND fecha < "2022-01-01"
   ')
   
   # -----
   # 2022
   # -----
   
   # Enero - Marzo
   
   def_2022 <- read.table("casos_hosp_uci_def_sexo_edad_provres.txt", 
                          header = TRUE, sep = ",", skip=116000)
   
   colnames(def_2022) <- colnames(def_16_2020)
   
   def_dia_2022 <- sqldf('
      SELECT fecha, SUM(num_casos) AS CASOS_DIA, SUM(num_def) AS DEF_dia
      FROM def_2022
      GROUP BY fecha
      HAVING fecha > "2021-12-31"
   ')
   
rm(def_17_2020, def_812_2020, def_16_2021, def_712_2021, def_2022)
   
fecha <- as.Date(c(def_dia_16_2020$fecha, def_dia_712_2020$fecha, 
                   def_dia_16_2021$fecha, def_dia_712_2021$fecha, 
                   def_dia_2022$fecha))
   
contagios <- c(def_dia_16_2020$CASOS_DIA, def_dia_712_2020$CASOS_DIA, 
               def_dia_16_2021$CASOS_DIA, def_dia_712_2021$CASOS_DIA, 
               def_dia_2022$CASOS_DIA)
   
defunciones <- c(def_dia_16_2020$DEF_dia, def_dia_712_2020$DEF_dia, 
                 def_dia_16_2021$DEF_dia, def_dia_712_2021$DEF_dia, 
                 def_dia_2022$DEF_dia)
   
contagios   <- data.frame (fecha = fecha, contagios = contagios)
defunciones <- data.frame (fecha=fecha,   defunciones=defunciones)


png(filename = "Diag_diarios.png", width = 550, height = 375)

plot(contagios, main="Gráfico 1. Diagnósticos diarios",
     ylab="nº diagnosticos", type= "l") + grid(nx=29)

dev.off()

png(filename = "Def_diarias.png", width = 550, height = 375)

plot(defunciones, main="Gráfico 2. Defunciones diarias",
     ylab="nº defunciones", type= "l") + grid(nx=29)

dev.off()
