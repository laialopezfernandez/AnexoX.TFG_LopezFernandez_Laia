# ------------------------------------------------------------------------------
# Anexo 4: Analisis de datos faltantes y division de las bases de datos
# ------------------------------------------------------------------------------

# Carga de las librerías
library(easypackages)
paq <-  c("visdat", "VIM", "readr"); libraries (paq)

# Importación de la Base de Datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/5. Barometros limpios")
Abril2020  <- read_delim("Abril2020_neta.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Enero2021  <- read_delim("Enero2021_neta.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Sept2021   <- read_delim("Sept2021_neta.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Feb2022    <- read_delim("Feb2022_neta.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Convertimos los -1 en NA
ToNA <- function (dd_neta){
  for(i in 1:nrow(dd_neta)){
    for(j in 1:ncol(dd_neta)){
      if(dd_neta[i,j]==-1){
        dd_neta[i,j]<- NA
      }
    }
  }
  return(dd_neta)
}

Abril2020 <- ToNA(Abril2020); Enero2021 <- ToNA(Enero2021)
Sept2021  <- ToNA(Sept2021) ; Feb2022   <- ToNA(Feb2022)



# 1.1. Analisis de datos faltantes
# ------------------------------------------------------------------------------
# Porcentaje de missings en cada variable
n1 <- dim(Abril2020)[1]; n2 <- dim(Enero2021)[1]
n3 <- dim(Sept2021)[1] ; n4 <- dim(Feb2022)[1]

colSums(is.na(Abril2020))/n1*100; colSums(is.na(Enero2021))/n2*100
colSums(is.na(Sept2021))/n3*100 ; colSums(is.na(Feb2022))/n4*100

# Analisis missings
vis_miss(Abril2020, sort_miss = TRUE) 
vis_miss(Enero2021, sort_miss = TRUE) 
vis_miss(Sept2021 , sort_miss = TRUE) 
vis_miss(Feb2022  , sort_miss = TRUE) 

aggr(Abril2020[,-1], numbers=T, sortVar=T, cex.axis=0.5)
aggr(Enero2021[,-1], numbers=T, sortVar=T, cex.axis=0.4) 
aggr(Sept2021[,-1] , numbers=T, sortVar=T, cex.axis=0.4) 
aggr(Feb2022[,-1]  , numbers=T, sortVar=T, cex.axis=0.4) 




# 1.2. Bases de datos completas
# ------------------------------------------------------------------------------
Abril2020_comp <- na.omit(Abril2020); n1_comp <- dim(Abril2020_comp)[1]
Enero2021_comp <- na.omit(Enero2021); n2_comp <- dim(Enero2021_comp)[1]
Sept2021_comp  <- na.omit(Sept2021) ; n3_comp <- dim(Sept2021_comp) [1]
Feb2022_comp   <- na.omit(Feb2022)  ; n4_comp <- dim(Feb2022_comp)  [1]



# 1.3. Asignación de la tipología de las variables
# ------------------------------------------------------------------------------
str(Abril2020); str(Enero2021); str(Sept2021); str(Feb2022)
# Por defecto son todas numericas

# Abril 2020
as_var_Abril2020 <- function(dd){
  
  dd$PROB_COVID <- factor(dd$PROB_COVID, labels = c("No", "Sí"))
  dd$PERS_COVID <- factor(dd$PERS_COVID, labels = c("No", "Sí"))
  
  dd$CCAA <- factor(dd$CCAA, labels = c("Andalucía", "Aragón", "Asturias", 
             "Baleares", "Canarias", "Cantabria", "Castilla la Mancha", 
             "Castilla y León", "Cataluña", "Comunitat Valenciana", 
             "Extremadura", "Galicia", "Madrid", "Murcia", "Navarra", 
             "País Vasco", "La Rioja", "Ceuta", "Melilla"))
  
  dd$TAMUNI <- factor(dd$TAMUNI, labels = c("<2.000", "(2.000, 10.000]", 
               "(10.000, 50.000]", "(50.000, 100.000]", "(100.000, 400.000]", 
               "(400.000, 1.000.000]", ">1.000.000"), order=TRUE)
  
  dd$SEXO <- factor(dd$SEXO, labels = c("Hombre", "Mujer"))
  
  dd$EDAD <- as.numeric(dd$EDAD)
  
  dd$EDAD.c <- factor(dd$EDAD.c, labels = c("de 18 a 24 años", 
               "de 25 a 34 años", "de 35 a 44 años", "de 45 a 54 años", 
               "de 55 a 64 años", "más de 65 años"), order=TRUE)
  
  dd$ESTADO_CIVIL <- factor(dd$ESTADO_CIVIL, labels = c("Casado/a", "Soltero/a", 
                                                        "Viudo/a", "Sep/Div"))
  
  dd$EXTR <- factor(dd$EXTR, labels = c("Española", "Doble u otra"))
  
  dd$ORIENT <- factor(dd$ORIENT, labels = c("Extrema Izda", "Izda", "Centro", 
                                            "Dcha", "Extrema Dcha"), order=TRUE)
  
  dd$ESTUD <- factor(dd$ESTUD, labels = c("Primaria o sin estudios", 
                                          "Secundarios", "Superiores"), order=TRUE)
  
  dd$RELIG <- factor(dd$RELIG, labels = c("Católico", "Otra religión", "Ateo/a"))
  
  dd$SIT_LAB <- factor(dd$SIT_LAB, labels = c("Trabaja", "Jub o Pens", 
                                              "Parado/a", "Estudiante", "Otros"))
  
  dd$SIT_ECON <- factor(dd$SIT_ECON, labels = c("Mala ", "Regular", "Buena"), 
                        order=TRUE)
  
  dd$CLASE <- factor(dd$CLASE, labels = c("Clase Baja", "Clase Media", 
                                          "Clase Alta"), order=TRUE)
  
  
  dd$PREOC <- factor(dd$PREOC, labels = c("Poco o nada", "Regular", 
                                          "Mucho y bastante"), order=TRUE)
  
  dd$OPT <- factor(dd$OPT, labels = c("Sí", "No"))
  
  dd$COMPGEN <- factor(dd$COMPGEN, labels = c("Bueno", "Malo"))
  
  return(dd)
  
}

Abril2020 <- as_var_Abril2020(Abril2020); str(Abril2020)

# Resto de barometros
as_var <- function(dd){
  
  if("OBLIG" == names(dd)[2]){
    dd$OBLIG <- factor(dd$OBLIG, labels = c("No", "Sí"))
  }
  
  dd$PROB_COVID <- factor(dd$PROB_COVID, labels = c("No", "Sí"))
  dd$PERS_COVID <- factor(dd$PERS_COVID, labels = c("No", "Sí"))

  dd$CCAA <- factor(dd$CCAA, labels = c("Andalucía", "Aragón", "Asturias", 
             "Baleares", "Canarias", "Cantabria", "Castilla la Mancha", 
             "Castilla y León", "Cataluña", "Comunitat Valenciana", 
             "Extremadura", "Galicia", "Madrid", "Murcia", "Navarra", 
             "País Vasco", "La Rioja", "Ceuta", "Melilla"))
  
  dd$TAMUNI <- factor(dd$TAMUNI, labels = c("<2.000", "(2.000, 10.000]", 
               "(10.000, 50.000]", "(50.000, 100.000]", "(100.000, 400.000]", 
               "(400.000, 1.000.000]", ">1.000.000"), order=TRUE)
  
  dd$SEXO <- factor(dd$SEXO, labels = c("Hombre", "Mujer"))
  
  dd$EDAD <- as.numeric(dd$EDAD)
  
  dd$EDAD.c <- factor(dd$EDAD.c, labels = c("de 18 a 24 años", 
               "de 25 a 34 años", "de 35 a 44 años", "de 45 a 54 años", 
               "de 55 a 64 años", "más de 65 años"), order=TRUE)
  
  dd$ESTADO_CIVIL <- factor(dd$ESTADO_CIVIL, labels = c("Casado/a", "Soltero/a", 
                                                        "Viudo/a", "Sep/Div"))
  
  dd$EXTR <- factor(dd$EXTR, labels = c("Española", "Doble u otra"))
  
  dd$ORIENT <- factor(dd$ORIENT, labels = c("Extrema Izda", "Izda", "Centro", 
                                 "Dcha", "Extrema Dcha"), order=TRUE)
  
  dd$ESTUD <- factor(dd$ESTUD, labels = c("Primaria o sin estudios", 
                               "Secundarios", "Superiores"), order=TRUE)
  
  dd$RELIG <- factor(dd$RELIG, labels = c("Católico", "Otra religión", "Ateo/a"))
  
  dd$SIT_LAB <- factor(dd$SIT_LAB, labels = c("Trabaja", "Jub o Pens", 
                                   "Parado/a", "Estudiante", "Otros"))
  
  dd$SIT_ECON <- factor(dd$SIT_ECON, labels = c("Mala ", "Regular", "Buena"), 
                        order=TRUE)
  
  dd$CLASE <- factor(dd$CLASE, labels = c("Clase Baja", "Clase Media", 
                                          "Clase Alta"), order=TRUE)
  
  
  
  dd$PREOC <- factor(dd$PREOC, labels = c("Poco o nada", "Regular", 
                                          "Mucho y bastante"), order=TRUE)
  
  dd$OPT <- factor(dd$OPT, labels = c("Sí", "No"))
  
  dd$COMPGEN <- factor(dd$COMPGEN, labels = c("Bueno", "Malo"))
  
  dd$AFEC_P <- factor(dd$AFEC_P, labels = c("No", "Sí"))
  
  dd$AF_ANIMICO <- factor(dd$AF_ANIMICO, labels = c("No ", "Sí"))
  
  dd$AF_SALUD <- factor(dd$AF_SALUD, labels = c("No", "Sí"))
  
  dd$AF_ECON <- factor(dd$AF_ECON, labels = c("No", "Sí"))
  
  dd$AF_LIBERTAD <- factor(dd$AF_LIBERTAD, labels = c("No", "Sí"))
  
  dd$AF_OTROS <- factor(dd$AF_OTROS, labels = c("No", "Sí"))
  
  
  dd$SINTOM <- factor(dd$SINTOM, labels = c("No", "Sí"))
  
  dd$DIAG <- factor(dd$DIAG, labels = c("No", "Sí"))
  
  dd$EVOL <- factor(dd$EVOL, labels = c("No covid o asimp", "Leve", "Importante, sH", "Importante, H"), order=TRUE)
  
  return(dd)
  
}
Enero2021 <- as_var(Enero2021); Sept2021  <- as_var(Sept2021)
Feb2022   <- as_var(Feb2022)

str(Enero2021); str(Sept2021); str(Feb2022)



# 1.4. Imputación de las bases de datos: 
# ------------------------------------------------------------------------------

dd_na   <- Abril2020 # Variar barometros
# dd_na <- Enero2021
# dd_na <- Sept2021
# dd_na <- Feb2022

# Codigo
library(mice)
init = mice(dd_na, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# Febrero 2022 y Septiembre 2021
# meth[c("EDAD")]="norm" 
# meth[c("OBLIG", "PROB_COVID", "PERS_COVID", "SEXO", "EXTR", "OPT", "AFEC_P", "AF_ANIMICO", "AF_SALUD", "AF_ECON", "AF_LIBERTAD", "AF_OTROS", "COMPGEN", "SINTOM", "DIAG")]="logreg" 
# meth[c("CCAA", "TAMUNI", "EDAD.c", "ESTADO_CIVIL", "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", "PREOC", "EVOL")]="polyreg"

# Enero 2021
# meth[c("EDAD")]="norm" 
# meth[c("PROB_COVID", "PERS_COVID", "SEXO", "EXTR", "OPT", "AFEC_P", "AF_ANIMICO", "AF_SALUD", "AF_ECON", "AF_LIBERTAD", "AF_OTROS", "COMPGEN", "SINTOM", "DIAG")]="logreg" 
# meth[c("CCAA", "TAMUNI", "EDAD.c", "ESTADO_CIVIL", "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", "PREOC", "EVOL")]="polyreg"

# Abril 2020
meth[c("EDAD")]="norm" 
meth[c("PROB_COVID", "PERS_COVID", "SEXO", "EXTR", "OPT", "COMPGEN")]="logreg" 
meth[c("CCAA", "TAMUNI", "EDAD.c", "ESTADO_CIVIL", "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", "PREOC")]="polyreg"

set.seed(2022)
imputed = mice(dd_na, method=meth, predictorMatrix=predM, m=5)
dd_imp <- complete (imputed)

Abril2020_imp   <- dd_imp # Variar barometros
# Enero2021_imp <- dd_imp 
# Sept2021_imp  <- dd_imp
# Feb2022_imp   <- dd_imp 

colSums(is.na(Abril2020_imp)); colSums(is.na(Enero2021_imp));
colSums(is.na(Sept2021_imp)) ; colSums(is.na(Feb2022_imp))  ;



# Exportamos en formato rds para conservar las clases
saveRDS(object = Abril2020_imp, file = "Abril2020_imp.rds")
saveRDS(object = Enero2021_imp, file = "Enero2021_imp.rds")
saveRDS(object = Sept2021_imp,  file = "Sept2021_imp.rds")
saveRDS(object = Feb2022_imp,   file = "Feb2022_imp.rds")

# Exportamos también en formato csv
write.csv2(Abril2020_imp, "Abril2020_imp.csv")
write.csv2(Enero2021_imp, "Enero2021_imp.csv")
write.csv2(Sept2021_imp,  "Sept2021_imp.csv")
write.csv2(Feb2022_imp,   "Feb2022_imp.csv")



# 1.6. División de la base de datos
# ------------------------------------------------------------------------------
Abril2020<- readRDS("Abril2020_imp.rds", refhook = NULL); 
Enero2021<- readRDS("Enero2021_imp.rds", refhook = NULL); 
Sept2021 <- readRDS("Sept2021_imp.rds", refhook = NULL) ; 
Feb2022  <- readRDS("Feb2022_imp.rds", refhook = NULL)  ; 

NA20 <- dim(Abril2020)[1]; NE21 <- dim(Enero2021)[1]
NS21 <- dim(Sept2021)[1] ; NF22 <- dim(Feb2022)[1]

set.seed(2022)
i <- sample.int(NA20, size=(round(0.15*NA20, 0) + 1), replace=FALSE)
dd_e_A20 <- Abril2020[i,]   # Test data Abril20
dd_m_A20 <- Abril2020[-i,]  # Training data Abril20

i <- sample.int(NE21, size=(round(0.15*NE21, 0) + 1), replace=FALSE)
dd_e_E21 <- Enero2021[i,]   # Test data Enero21
dd_m_E21 <- Enero2021[-i,]  # Training data Enero21

i <- sample.int(NS21, size=(round(0.15*NS21, 0) + 1), replace=FALSE)
dd_e_S21 <- Sept2021[i,]   # Test data Sept21
dd_m_S21 <- Sept2021[-i,]  # Training data Sept21

i <- sample.int(NF22, size=(round(0.15*NF22, 0) + 1), replace=FALSE)
dd_e_F22 <- Feb2022[i,]   # Test data Feb22
dd_m_F22 <- Feb2022[-i,]  # Training data Feb22

