# ------------------------------------------------------------------------------
# Anexo 3: Pre-processing
# ------------------------------------------------------------------------------



# Anexo 3.1: Pre-processing del barómetro de Febrero de 2022
# ------------------------------------------------------------------------------
# Carga de las librerías
library(easypackages)
paq <-  c("readr"); libraries (paq)

# Importación de la Base de Datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/4. Feb 2022")
Feb2022 <- read_delim("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/4. Feb 2022/Feb2022.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Nombre variables base original
names <- names(Feb2022)

# Seleccionamos las variables que se van a utilizar i cambiamos el nombre

var <- c("P8", "PESPANNA1", "PPERSONAL1", 
         "CCAA", "TAMUNI", "SEXO", "EDAD", "ECIVIL", "P0", "ESCIDEOL", 
         "ESTUDIOS", "RELIGIONR", "SITLAB", "ECOPER", "CLASESOCIAL", 
         "P1", "P4", "P11", "P13A_1", "P13A_2", "P13A_3", "P13A_4", "P13A_5", 
         "P13A_6", "P13A_7", "P13A_8", "P13A_9", "P13A_10", "P13A_96", 
         "P13A_98","P13A_99", "P10", 
         "SERSANIENTRE", "CORONAENTRE", "EVOLUCENTRE")


Feb2022 <- Feb2022[,var]

names(Feb2022) <- c("OBLIG", "PROB_COVID1", "PERS_COVID1", 
                    "CCAA", "TAMUNI", "SEXO", "EDAD", "ESTADO_CIVIL", "EXTR", 
                    "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
                    "PREOC", "OPT", "AFEC_P", "P13A_1", "P13A_2", "P13A_3", 
                    "P13A_4", "P13A_5", "P13A_6", "P13A_7", "P13A_8", "P13A_9",
                    "P13A_10", "P13A_96", "P13A_98","P13A_99", "COMPGEN", 
                    "SINTOM", "DIAG", "EVOL")


# Variables endógenas
# ------------------------------------------------------------------------------
# OBLIG: Obligatoriedad de las vacunas
# Feb2022$OBLIG2 <- Feb2022$OBLIG
for(i in 1:length(Feb2022$OBLIG)){
  if(Feb2022$OBLIG[i]==1 | Feb2022$OBLIG[i]==3){Feb2022$OBLIG[i]<-1} # Sí
  if(Feb2022$OBLIG[i]==2){Feb2022$OBLIG[i]<-0}                       # No
  if(Feb2022$OBLIG[i]==8 | Feb2022$OBLIG[i]==9){Feb2022$OBLIG[i]<--1}# NS/NC
}
# table(Feb2022$OBLIG); table(Feb2022$OBLIG2) # OK! Ha funcionado bien

# PROB_COVID: Primer problema de españa es el COVID o alguno relacionado con él 
# (categorias 53, 59, 60, 61, 62, 63, 64, 67, 68 y 69)
# PERS_COVID: Primer problema personal  es el COVID o alguno relacionado con él

# Feb2022$PROB_COVID1.2 <- Feb2022$PROB_COVID1
# Feb2022$PERS_COVID1.2 <- Feb2022$PERS_COVID1
Feb2022$PROB_COVID <- rep(0, length(Feb2022$PROB_COVID1))
Feb2022$PERS_COVID <- rep(0, length(Feb2022$PROB_COVID1))

for(i in 1:length(Feb2022$PROB_COVID)){
  if(Feb2022$PROB_COVID1[i]=="53" | Feb2022$PROB_COVID1[i]=="59" | 
     Feb2022$PROB_COVID1[i]=="60" | Feb2022$PROB_COVID1[i]=="61" | 
     Feb2022$PROB_COVID1[i]=="62" | Feb2022$PROB_COVID1[i]=="63" |
     Feb2022$PROB_COVID1[i]=="64" | Feb2022$PROB_COVID1[i]=="67" | 
     Feb2022$PROB_COVID1[i]=="68" | Feb2022$PROB_COVID1[i]=="69") 
    
  { Feb2022$PROB_COVID[i]<- 1 }
  
  if(Feb2022$PERS_COVID1[i]=="53" | Feb2022$PERS_COVID1[i]=="59" | 
     Feb2022$PERS_COVID1[i]=="60" | Feb2022$PERS_COVID1[i]=="61" | 
     Feb2022$PERS_COVID1[i]=="62" | Feb2022$PERS_COVID1[i]=="63" |
     Feb2022$PERS_COVID1[i]=="64" | Feb2022$PERS_COVID1[i]=="67" | 
     Feb2022$PERS_COVID1[i]=="68" | Feb2022$PERS_COVID1[i]=="69") 
    
  { Feb2022$PERS_COVID[i]<- 1 }
  
}
# table(Feb2022$PROB_COVID1.2); table(Feb2022$PERS_COVID1.2)
# table(Feb2022$PROB_COVID); table(Feb2022$PERS_COVID)
# OK! Ha funcionado bien


# Variables exógenas de tipo socioeconómico
# ------------------------------------------------------------------------------
# SEXO
Feb2022$SEXO <- Feb2022$SEXO - 1 # 0: Hombre
                                 # 1: Mujer

# EDAD
Feb2022$EDAD.c <- c()
for(i in 1:length(Feb2022$EDAD)){
  if(Feb2022$EDAD[i]<=24){Feb2022$EDAD.c[i] <- 1}
  if(Feb2022$EDAD[i]>24 && Feb2022$EDAD[i]<=34){Feb2022$EDAD.c[i] <- 2}
  if(Feb2022$EDAD[i]>34 && Feb2022$EDAD[i]<=44){Feb2022$EDAD.c[i] <- 3}
  if(Feb2022$EDAD[i]>44 && Feb2022$EDAD[i]<=54){Feb2022$EDAD.c[i] <- 4}
  if(Feb2022$EDAD[i]>54 && Feb2022$EDAD[i]<=64){Feb2022$EDAD.c[i] <- 5}
  if(Feb2022$EDAD[i]>=65){Feb2022$EDAD.c[i] <- 6}
}
# table(Feb2022$EDAD.c); table(Feb2022$EDAD) # OK! Ha funcionado bien

# ESTADO_CIVIL
# Feb2022$ESTADO_CIVIL2 <- Feb2022$ESTADO_CIVIL
for(i in 1:length(Feb2022$ESTADO_CIVIL)){
  if(Feb2022$ESTADO_CIVIL[i]==5){Feb2022$ESTADO_CIVIL[i] <- 4}
  if(Feb2022$ESTADO_CIVIL[i]==9){Feb2022$ESTADO_CIVIL[i] <- -1}
}
# table(Feb2022$ESTADO_CIVIL); table(Feb2022$ESTADO_CIVIL2) 
# OK! Ha funcionado bien

# ESTR
# Feb2022$EXTR2 <- Feb2022$EXTR
for(i in 1:length(Feb2022$EXTR)){
  if(Feb2022$EXTR[i]==1){Feb2022$EXTR[i] <- 0} # Españoles
  if(Feb2022$EXTR[i]==2 | Feb2022$EXTR[i]==3) {Feb2022$EXTR[i] <- 1}
  # Extranjeros o doble nacionalidad
}
# table(Feb2022$EXTR); table(Feb2022$EXTR2) 
# OK! Ha funcionado bien

# ORIENT
# Feb2022$ORIENT2 <- Feb2022$ORIENT
for(i in 1:length(Feb2022$ORIENT)){
  if(Feb2022$ORIENT[i]==1  | Feb2022$ORIENT[i]==2) {Feb2022$ORIENT[i] <- 1}
  if(Feb2022$ORIENT[i]==3  | Feb2022$ORIENT[i]==4) {Feb2022$ORIENT[i] <- 2}
  if(Feb2022$ORIENT[i]==5  | Feb2022$ORIENT[i]==6) {Feb2022$ORIENT[i] <- 3}
  if(Feb2022$ORIENT[i]==7  | Feb2022$ORIENT[i]==8) {Feb2022$ORIENT[i] <- 4}
  if(Feb2022$ORIENT[i]==9  | Feb2022$ORIENT[i]==10){Feb2022$ORIENT[i] <- 5}
  if(Feb2022$ORIENT[i]==98 | Feb2022$ORIENT[i]==99){Feb2022$ORIENT[i] <- -1}
}
# table(Feb2022$ORIENT); table(Feb2022$ORIENT2) # OK! Ha funcionado bien

# ESTUD
# Feb2022$ESTUD2 <- Feb2022$ESTUD
for(i in 1:length(Feb2022$ESTUD)){
  if(Feb2022$ESTUD[i]==1  | Feb2022$ESTUD[i]==2) {Feb2022$ESTUD[i] <- 1}
  if(Feb2022$ESTUD[i]==3  | Feb2022$ESTUD[i]==4 | Feb2022$ESTUD[i]==5  ) 
    {Feb2022$ESTUD[i] <- 2}
  if(Feb2022$ESTUD[i]==6) {Feb2022$ESTUD[i] <- 3}
  if(Feb2022$ESTUD[i]==7  | Feb2022$ESTUD[i]==9) {Feb2022$ESTUD[i] <- -1}
}
# table(Feb2022$ESTUD); table(Feb2022$ESTUD2) # OK! Ha funcionado bien

# RELIG
# Feb2022$RELIG2 <- Feb2022$RELIG
for(i in 1:length(Feb2022$RELIG)){
  if(Feb2022$RELIG[i]==1  | Feb2022$RELIG[i]==2) {Feb2022$RELIG[i] <- 1} # Católico
  if(Feb2022$RELIG[i]==3) {Feb2022$RELIG[i] <- 2} # Creyente de otra religión
  if(Feb2022$RELIG[i]==4  | Feb2022$RELIG[i]==5 |
     Feb2022$RELIG[i]==6) {Feb2022$RELIG[i] <- 3} # Agnóstico, indiferente, ateo/a
  if(Feb2022$RELIG[i]==9) {Feb2022$RELIG[i] <- -1} # NS/NC
}
# table(Feb2022$RELIG); table(Feb2022$RELIG2) # OK! Ha funcionado bien

# SIT_LAB
# Feb2022$SIT_LAB2 <- Feb2022$SIT_LAB
for(i in 1:length(Feb2022$SIT_LAB)){
  if(Feb2022$SIT_LAB[i]==1) {Feb2022$SIT_LAB[i] <- 1} # Trabaja
  if(Feb2022$SIT_LAB[i]==2  | Feb2022$SIT_LAB[i]==3) 
    {Feb2022$SIT_LAB[i] <- 2} # Jubilado o Pensionista
  if(Feb2022$SIT_LAB[i]==4  | Feb2022$SIT_LAB[i]==5) 
    {Feb2022$SIT_LAB[i] <- 3} # Parado
  if(Feb2022$SIT_LAB[i]==6) {Feb2022$SIT_LAB[i] <- 4} 
  # Estudiante
  if(Feb2022$SIT_LAB[i]==7  | Feb2022$SIT_LAB[i]==8) 
    {Feb2022$SIT_LAB[i] <- 5} # Otros
  if(Feb2022$SIT_LAB[i]==9) {Feb2022$SIT_LAB[i] <- -1} # NS/NC
}
# table(Feb2022$SIT_LAB); table(Feb2022$SIT_LAB2) # OK! Ha funcionado bien

# SIT_ECON
# Feb2022$SIT_ECON2 <- Feb2022$SIT_ECON
for(i in 1:length(Feb2022$SIT_ECON)){
  if(Feb2022$SIT_ECON[i]==1  | Feb2022$SIT_ECON[i]==2) 
    {Feb2022$SIT_ECON[i] <- 3} # Buena o muy buena
  else if(Feb2022$SIT_ECON[i]==3) {Feb2022$SIT_ECON[i] <- 2} # Regular
  else if(Feb2022$SIT_ECON[i]==4  | Feb2022$SIT_ECON[i]==5) 
    {Feb2022$SIT_ECON[i] <- 1} # Mala o muy mala
  else if(Feb2022$SIT_ECON[i]==8  | Feb2022$SIT_ECON[i]==9) 
    {Feb2022$SIT_ECON[i] <- -1} # NS/NC
}
# table(Feb2022$SIT_ECON); table(Feb2022$SIT_ECON2) # OK! Ha funcionado bien

# CLASE
# Feb2022$CLASE2 <- Feb2022$CLASE
for(i in 1:length(Feb2022$CLASE)){
  
  if(Feb2022$CLASE[i]==1  | Feb2022$CLASE[i]==2) 
    {Feb2022$CLASE[i] <- 3} # Clase alta y media alta
  
  else if(Feb2022$CLASE[i]==3 | Feb2022$CLASE[i]==11) 
    {Feb2022$CLASE[i] <- 2} # Clase Media
  
  else if(Feb2022$CLASE[i]==4 | Feb2022$CLASE[i]==5 | 
          Feb2022$CLASE[i]==6 | Feb2022$CLASE[i]==7 | 
          Feb2022$CLASE[i]==8 | Feb2022$CLASE[i]==9 | 
          Feb2022$CLASE[i]==12) {Feb2022$CLASE[i] <- 1} # Clase Baja
  
  else if(Feb2022$CLASE[i]==10 | Feb2022$CLASE[i]==96 | 
          Feb2022$CLASE[i]==97 | Feb2022$CLASE[i]==98 | 
          Feb2022$CLASE[i]==99 ) {Feb2022$CLASE[i] <- -1} # NS/NC
}
# table(Feb2022$CLASE); table(Feb2022$CLASE2) # OK! Ha funcionado bien


# Variables exógenas que miden el grado de preocupación
# ------------------------------------------------------------------------------
# PREOC
# Feb2022$PREOC2 <- Feb2022$PREOC
for(i in 1:length(Feb2022$PREOC)){
  if(Feb2022$PREOC[i]==1  | Feb2022$PREOC[i]==2) 
    {Feb2022$PREOC[i] <- 3} # Mucho y bastante
  
  else if(Feb2022$PREOC[i]==3) {Feb2022$PREOC[i] <- 2} # Regular
  
  else if(Feb2022$PREOC[i]==4 | Feb2022$PREOC[i]==5) 
    {Feb2022$PREOC[i] <- 1} # Poco o nada
  
  else if(Feb2022$PREOC[i]==8 | Feb2022$PREOC[i]==9) 
    {Feb2022$PREOC[i] <- -1} # NS/NC
}
# table(Feb2022$PREOC); table(Feb2022$PREOC2) # OK! Ha funcionado bien

# OPT
# Feb2022$OPT2 <- Feb2022$OPT
for(i in 1:length(Feb2022$OPT)){
  if(Feb2022$OPT[i]==1) {Feb2022$OPT[i] <- 0} # Lo peor ha pasado ya
  else if(Feb2022$OPT[i]==2 | Feb2022$OPT[i]==3) 
    {Feb2022$OPT[i] <- 1} # Seguimos en lo peor o está por llegar
  else if(Feb2022$OPT[i]==8 | Feb2022$OPT[i]==9) 
    {Feb2022$OPT[i] <- -1} # NS/NC
}
# table(Feb2022$OPT); table(Feb2022$OPT2) # OK! Ha funcionado bien

# COMPGEN
# Feb2022$COMPGEN2 <- Feb2022$COMPGEN
for(i in 1:length(Feb2022$COMPGEN)){
  if(Feb2022$COMPGEN[i]==1) {Feb2022$COMPGEN[i] <- 0} 
  # Cree la mayoría está reaccionando con civismo y solidaridad
  else if(Feb2022$COMPGEN[i]==2) {Feb2022$COMPGEN[i] <- 1} 
  # Mayoría poco cívica e indisciplinada
  else if(Feb2022$COMPGEN[i]==8 | Feb2022$COMPGEN[i]==9) 
    {Feb2022$COMPGEN[i] <- -1} # NS/NC
}
# table(Feb2022$COMPGEN); table(Feb2022$COMPGEN2) # OK! Ha funcionado bien

# AFEC_P
# Feb2022$AFEC_P2 <- Feb2022$AFEC_P
for(i in 1:length(Feb2022$AFEC_P)){
  if(Feb2022$AFEC_P[i]==1 | Feb2022$AFEC_P[i]==2) 
    {Feb2022$AFEC_P[i] <- 1} # La pandemia ha afectado a su vida personal
  else if(Feb2022$AFEC_P[i]==3 | Feb2022$AFEC_P[i]==4 | 
          Feb2022$AFEC_P[i]==5) {Feb2022$AFEC_P[i] <- 0} 
  # La pandemia no ha afectado a su vida personal
  else if(Feb2022$AFEC_P[i]==8 | Feb2022$AFEC_P[i]==9) 
    {Feb2022$AFEC_P[i] <- -1} # NS/NC
}
# table(Feb2022$AFEC_P); table(Feb2022$AFEC_P2) # OK! Ha funcionado bien
# AF_ANIMICO (cat 1, 3 y 7), AF_SALUD (cat 5), AF_ECON (cat 2, 6 y 9), 
# AF_LIBERTAD (cat 4, 8 y 10), AF_OTROS(cat 96)

# Convierto los NA en 0, no menciona
for (i in 1:length(Feb2022$P13A_1)){
  if(is.na(Feb2022$P13A_1[i])) {Feb2022$P13A_1[i] <- 0}
  if(is.na(Feb2022$P13A_2[i])) {Feb2022$P13A_2[i] <- 0}
  if(is.na(Feb2022$P13A_3[i])) {Feb2022$P13A_3[i] <- 0}
  if(is.na(Feb2022$P13A_4[i])) {Feb2022$P13A_4[i] <- 0}
  if(is.na(Feb2022$P13A_5[i])) {Feb2022$P13A_5[i] <- 0}
  if(is.na(Feb2022$P13A_6[i])) {Feb2022$P13A_6[i] <- 0}
  if(is.na(Feb2022$P13A_7[i])) {Feb2022$P13A_7[i] <- 0}
  if(is.na(Feb2022$P13A_8[i])) {Feb2022$P13A_8[i] <- 0}
  if(is.na(Feb2022$P13A_9[i])) {Feb2022$P13A_9[i] <- 0}
  if(is.na(Feb2022$P13A_10[i])){Feb2022$P13A_10[i] <- 0}
  if(is.na(Feb2022$P13A_96[i])){Feb2022$P13A_96[i] <- 0}
  if(is.na(Feb2022$P13A_98[i])){Feb2022$P13A_98[i] <- 0}
  if(is.na(Feb2022$P13A_99[i])){Feb2022$P13A_99[i] <- 0}
}

# Inicializo en 0 (no ha afectado o no menciona)
Feb2022$AF_ANIMICO  <- rep(0, length(Feb2022$P13A_1))
Feb2022$AF_SALUD    <- rep(0, length(Feb2022$P13A_1))
Feb2022$AF_ECON     <- rep(0, length(Feb2022$P13A_1))
Feb2022$AF_LIBERTAD <- rep(0, length(Feb2022$P13A_1))
Feb2022$AF_OTROS    <- rep(0, length(Feb2022$P13A_1))

for (i in 1:length(Feb2022$P13A_1)){
  # AF_ANIMICO (1, 3 y 7)
  if(Feb2022$P13A_1[i]==1 | Feb2022$P13A_3[i]==1 | Feb2022$P13A_7[i]==1) 
  {Feb2022$AF_ANIMICO[i] <- 1}
  # AF_SALUD (5)
  if(Feb2022$P13A_5[i]==1) {Feb2022$AF_SALUD[i] <- 1}
  # AF_ECON (2, 6 y 9)
  if(Feb2022$P13A_2[i]==1 | Feb2022$P13A_6[i]==1 | Feb2022$P13A_9[i]==1) 
  {Feb2022$AF_ECON[i] <- 1}
  # AF_LIBERTAD (4, 8 y 10)
  if(Feb2022$P13A_4[i]==1 | Feb2022$P13A_8[i]==1 | Feb2022$P13A_10[i]==1) 
  {Feb2022$AF_LIBERTAD[i] <- 1}
  # AF_OTROS (96)
  if(Feb2022$P13A_96[i]==1) {Feb2022$AF_OTROS[i] <- 1}
}


# Variables exógenas enfermedad
# ------------------------------------------------------------------------------
# SINTOM
# Feb2022$SINTOM2 <- Feb2022$SINTOM
for(i in 1:length(Feb2022$SINTOM)){
  if(Feb2022$SINTOM[i]==3) {Feb2022$SINTOM[i] <- 1} # Ha tenido síntomas
  else if(Feb2022$SINTOM[i]==2) {Feb2022$SINTOM[i] <- 0} # No ha tenido síntomas
  else if(Feb2022$SINTOM[i]==9) {Feb2022$SINTOM[i] <- -1} # NS/NC
}
# table(Feb2022$SINTOM); table(Feb2022$SINTOM2) # OK! Ha funcionado bien

# DIAG i EVOL
# Feb2022$DIAG2 <- Feb2022$DIAG
# Feb2022$EVOL2 <- Feb2022$EVOL

for (i in 1:length(Feb2022$DIAG)){
  if(is.na(Feb2022$DIAG[i])){Feb2022$DIAG[i] <-0}
  else if(Feb2022$DIAG[i]==1){Feb2022$DIAG[i]<-1}
  else if(Feb2022$DIAG[i]==2){Feb2022$DIAG[i]<-0}
  else if(Feb2022$DIAG[i]==9){Feb2022$DIAG[i]<-0}
}

for (i in 1:length(Feb2022$EVOL)){
  if(is.na(Feb2022$EVOL[i])){Feb2022$EVOL[i]<-1} 
  else if(Feb2022$EVOL[i]==4){Feb2022$EVOL[i]<-1} 
  # No se ha contagiado o ha sido asintomático
  
  else if(Feb2022$EVOL[i]==1){Feb2022$EVOL[i]<-2} 
  # Diagnóstico favorable, con síntomas leves
  
  else if(Feb2022$EVOL[i]==2){Feb2022$EVOL[i]<-3} 
  # Diagnóstico favorable, síntomas importantes sin hospitalización
  
  else if(Feb2022$EVOL[i]==3){Feb2022$EVOL[i]<-4} 
  # Diagnóstico favorable, hospitalización
  
  else if(Feb2022$EVOL[i]==9){Feb2022$EVOL[i]<--1} # NS/NC
}

# table(Feb2022$DIAG); table(Feb2022$DIAG2) # OK! Ha funcionado bien
# table(Feb2022$EVOL); table(Feb2022$EVOL2) # OK! Ha funcionado bien


# Descargamos la base de datos arreglada
vars <- c("OBLIG", "PROB_COVID", "PERS_COVID", 
          "CCAA", "TAMUNI", "SEXO", "EDAD", "EDAD.c", "ESTADO_CIVIL", "EXTR",
          "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
          "PREOC", "OPT", "AFEC_P", "AF_ANIMICO", "AF_SALUD", "AF_ECON", 
          "AF_LIBERTAD", "AF_OTROS", "COMPGEN", 
          "SINTOM", "DIAG", "EVOL")

dd_neta <- as.data.frame(Feb2022[,vars])

# Descargamos en CSV (o xlsx)
# write.csv2(dd_neta, "Feb2022_neta.csv")





# Anexo 3.2: Pre-processing del barómetro de Septiembre de 2021
# ------------------------------------------------------------------------------
# Importación de la Base de Datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/3. Sept 2021")
Sept2021 <- read_delim("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/3. Sept 2021/Sept2021.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Nombre variables base original
names <- names(Sept2021)

# Seleccionamos las variables que se van a utilizar i cambiamos el nombre

var <- c("P11", "PESPANNA1", "PPERSONAL1", 
         "CCAA", "TAMUNI", "SEXO", "EDAD", "ECIVIL", "P0", "ESCIDEOL", 
         "ESTUDIOS", "RELIGIONR", "SITLAB", "P15", "CLASESOCIAL", 
         "P1", "P4", "P8", "P10A_1", "P10A_2", "P10A_3", "P10A_4", "P10A_5", 
         "P10A_6", "P10A_7", "P10A_8", "P10A_9", "P10A_10", "P10A_96", 
         "P10A_98","P10A_99", "P7", 
         "SERSANIENTRE", "CORONAENTRE", "EVOLUCENTRE")


Sept2021 <- Sept2021[,var]

names(Sept2021) <- c("OBLIG", "PROB_COVID1", "PERS_COVID1", 
                    "CCAA", "TAMUNI", "SEXO", "EDAD", "ESTADO_CIVIL", "EXTR", 
                    "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
                    "PREOC", "OPT", "AFEC_P", "P10A_1", "P10A_2", "P10A_3", 
                    "P10A_4", "P10A_5", "P10A_6", "P10A_7", "P10A_8", "P10A_9",
                    "P10A_10", "P10A_96", "P10A_98","P10A_99", "COMPGEN", 
                    "SINTOM", "DIAG", "EVOL")


# Variables endógenas
# ------------------------------------------------------------------------------
# OBLIG: Obligatoriedad de las vacunas
# Sept2021$OBLIG2 <- Sept2021$OBLIG
for(i in 1:length(Sept2021$OBLIG)){
  if(Sept2021$OBLIG[i]==1 | Sept2021$OBLIG[i]==3){Sept2021$OBLIG[i]<-1} # Sí
  if(Sept2021$OBLIG[i]==2){Sept2021$OBLIG[i]<-0}                       # No
  if(Sept2021$OBLIG[i]==8 | Sept2021$OBLIG[i]==9){Sept2021$OBLIG[i]<--1}# NS/NC
}
# table(Sept2021$OBLIG); table(Sept2021$OBLIG2) # OK! Ha funcionado bien

# PROB_COVID: Primer problema de españa es el COVID o alguno relacionado con él 
# (categorias 53, 59, 60, 61, 62, 63, 64, 67, 68 y 69)
# PERS_COVID: Primer problema personal  es el COVID o alguno relacionado con él

# Sept2021$PROB_COVID1.2 <- Sept2021$PROB_COVID1
# Sept2021$PERS_COVID1.2 <- Sept2021$PERS_COVID1
Sept2021$PROB_COVID <- rep(0, length(Sept2021$PROB_COVID1))
Sept2021$PERS_COVID <- rep(0, length(Sept2021$PROB_COVID1))

for(i in 1:length(Sept2021$PROB_COVID)){
  if(Sept2021$PROB_COVID1[i]=="53" | Sept2021$PROB_COVID1[i]=="59" | 
     Sept2021$PROB_COVID1[i]=="60" | Sept2021$PROB_COVID1[i]=="61" | 
     Sept2021$PROB_COVID1[i]=="62" | Sept2021$PROB_COVID1[i]=="63" |
     Sept2021$PROB_COVID1[i]=="64" | Sept2021$PROB_COVID1[i]=="67" | 
     Sept2021$PROB_COVID1[i]=="68" | Sept2021$PROB_COVID1[i]=="69") 
    
  { Sept2021$PROB_COVID[i]<- 1 }
  
  if(Sept2021$PERS_COVID1[i]=="53" | Sept2021$PERS_COVID1[i]=="59" | 
     Sept2021$PERS_COVID1[i]=="60" | Sept2021$PERS_COVID1[i]=="61" | 
     Sept2021$PERS_COVID1[i]=="62" | Sept2021$PERS_COVID1[i]=="63" |
     Sept2021$PERS_COVID1[i]=="64" | Sept2021$PERS_COVID1[i]=="67" | 
     Sept2021$PERS_COVID1[i]=="68" | Sept2021$PERS_COVID1[i]=="69") 
    
  { Sept2021$PERS_COVID[i]<- 1 }
  
}
# table(Sept2021$PROB_COVID1.2); table(Sept2021$PERS_COVID1.2)
# table(Sept2021$PROB_COVID); table(Sept2021$PERS_COVID)
# OK! Ha funcionado bien


# Variables exógenas de tipo socioeconómico
# ------------------------------------------------------------------------------
# SEXO
Sept2021$SEXO <- Sept2021$SEXO - 1 # 0: Hombre
                                   # 1: Mujer

# EDAD
Sept2021$EDAD.c <- c()
for(i in 1:length(Sept2021$EDAD)){
  if(Sept2021$EDAD[i]<=24){Sept2021$EDAD.c[i] <- 1}
  if(Sept2021$EDAD[i]>24 && Sept2021$EDAD[i]<=34){Sept2021$EDAD.c[i] <- 2}
  if(Sept2021$EDAD[i]>34 && Sept2021$EDAD[i]<=44){Sept2021$EDAD.c[i] <- 3}
  if(Sept2021$EDAD[i]>44 && Sept2021$EDAD[i]<=54){Sept2021$EDAD.c[i] <- 4}
  if(Sept2021$EDAD[i]>54 && Sept2021$EDAD[i]<=64){Sept2021$EDAD.c[i] <- 5}
  if(Sept2021$EDAD[i]>=65){Sept2021$EDAD.c[i] <- 6}
}
# table(Sept2021$EDAD.c); table(Sept2021$EDAD) # OK! Ha funcionado bien

# ESTADO_CIVIL
# Sept2021$ESTADO_CIVIL2 <- Sept2021$ESTADO_CIVIL
for(i in 1:length(Sept2021$ESTADO_CIVIL)){
  if(Sept2021$ESTADO_CIVIL[i]==5){Sept2021$ESTADO_CIVIL[i] <- 4}
  if(Sept2021$ESTADO_CIVIL[i]==9){Sept2021$ESTADO_CIVIL[i] <- -1}
}
# table(Sept2021$ESTADO_CIVIL); table(Sept2021$ESTADO_CIVIL2) 
# OK! Ha funcionado bien

# ESTR
# Sept2021$EXTR2 <- Sept2021$EXTR
for(i in 1:length(Sept2021$EXTR)){
  if(Sept2021$EXTR[i]==1){Sept2021$EXTR[i] <- 0} # Españoles
  if(Sept2021$EXTR[i]==2 | Sept2021$EXTR[i]==3) {Sept2021$EXTR[i] <- 1}
  # Extranjeros o doble nacionalidad
}
# table(Sept2021$EXTR); table(Sept2021$EXTR2) 
# OK! Ha funcionado bien

# ORIENT
# Sept2021$ORIENT2 <- Sept2021$ORIENT
for(i in 1:length(Sept2021$ORIENT)){
  if(Sept2021$ORIENT[i]==1  | Sept2021$ORIENT[i]==2) {Sept2021$ORIENT[i] <- 1}
  if(Sept2021$ORIENT[i]==3  | Sept2021$ORIENT[i]==4) {Sept2021$ORIENT[i] <- 2}
  if(Sept2021$ORIENT[i]==5  | Sept2021$ORIENT[i]==6) {Sept2021$ORIENT[i] <- 3}
  if(Sept2021$ORIENT[i]==7  | Sept2021$ORIENT[i]==8) {Sept2021$ORIENT[i] <- 4}
  if(Sept2021$ORIENT[i]==9  | Sept2021$ORIENT[i]==10){Sept2021$ORIENT[i] <- 5}
  if(Sept2021$ORIENT[i]==98 | Sept2021$ORIENT[i]==99){Sept2021$ORIENT[i] <- -1}
}
# table(Sept2021$ORIENT); table(Sept2021$ORIENT2) # OK! Ha funcionado bien

# ESTUD
# Sept2021$ESTUD2 <- Sept2021$ESTUD
for(i in 1:length(Sept2021$ESTUD)){
  if(Sept2021$ESTUD[i]==1  | Sept2021$ESTUD[i]==2) {Sept2021$ESTUD[i] <- 1}
  if(Sept2021$ESTUD[i]==3  | Sept2021$ESTUD[i]==4 | Sept2021$ESTUD[i]==5  ) 
  {Sept2021$ESTUD[i] <- 2}
  if(Sept2021$ESTUD[i]==6) {Sept2021$ESTUD[i] <- 3}
  if(Sept2021$ESTUD[i]==7  | Sept2021$ESTUD[i]==9) {Sept2021$ESTUD[i] <- -1}
}
# table(Sept2021$ESTUD); table(Sept2021$ESTUD2) # OK! Ha funcionado bien

# RELIG
# Sept2021$RELIG2 <- Sept2021$RELIG
for(i in 1:length(Sept2021$RELIG)){
  if(Sept2021$RELIG[i]==1  | Sept2021$RELIG[i]==2) {Sept2021$RELIG[i] <- 1} # Católico
  if(Sept2021$RELIG[i]==3) {Sept2021$RELIG[i] <- 2} # Creyente de otra religión
  if(Sept2021$RELIG[i]==4  ) {Sept2021$RELIG[i] <- 3} # Agnóstico, indiferente, ateo/a
  if(Sept2021$RELIG[i]==9) {Sept2021$RELIG[i] <- -1} # NS/NC
}
# table(Sept2021$RELIG); table(Sept2021$RELIG2) # OK! Ha funcionado bien

# SIT_LAB
# Sept2021$SIT_LAB2 <- Sept2021$SIT_LAB
for(i in 1:length(Sept2021$SIT_LAB)){
  if(Sept2021$SIT_LAB[i]==1) {Sept2021$SIT_LAB[i] <- 1} # Trabaja
  if(Sept2021$SIT_LAB[i]==2  | Sept2021$SIT_LAB[i]==3) 
  {Sept2021$SIT_LAB[i] <- 2} # Jubilado o Pensionista
  if(Sept2021$SIT_LAB[i]==4  | Sept2021$SIT_LAB[i]==5) 
  {Sept2021$SIT_LAB[i] <- 3} # Parado
  if(Sept2021$SIT_LAB[i]==6) {Sept2021$SIT_LAB[i] <- 4} 
  # Estudiante
  if(Sept2021$SIT_LAB[i]==7  | Sept2021$SIT_LAB[i]==8) 
  {Sept2021$SIT_LAB[i] <- 5} # Otros
  if(Sept2021$SIT_LAB[i]==9) {Sept2021$SIT_LAB[i] <- -1} # NS/NC
}
# table(Sept2021$SIT_LAB); table(Sept2021$SIT_LAB2) # OK! Ha funcionado bien

# SIT_ECON
# Sept2021$SIT_ECON2 <- Sept2021$SIT_ECON
for(i in 1:length(Sept2021$SIT_ECON)){
  if(Sept2021$SIT_ECON[i]==1  | Sept2021$SIT_ECON[i]==2) 
  {Sept2021$SIT_ECON[i] <- 3} # Buena o muy buena
  else if(Sept2021$SIT_ECON[i]==3) {Sept2021$SIT_ECON[i] <- 2} # Regular
  else if(Sept2021$SIT_ECON[i]==4  | Sept2021$SIT_ECON[i]==5) 
  {Sept2021$SIT_ECON[i] <- 1} # Mala o muy mala
  else if(Sept2021$SIT_ECON[i]==8  | Sept2021$SIT_ECON[i]==9) 
  {Sept2021$SIT_ECON[i] <- -1} # NS/NC
}
# table(Sept2021$SIT_ECON); table(Sept2021$SIT_ECON2) # OK! Ha funcionado bien

# CLASE
# Sept2021$CLASE2 <- Sept2021$CLASE
for(i in 1:length(Sept2021$CLASE)){
  
  if(Sept2021$CLASE[i]==1  | Sept2021$CLASE[i]==2) 
  {Sept2021$CLASE[i] <- 3} # Clase alta y media alta
  
  else if(Sept2021$CLASE[i]==3 | Sept2021$CLASE[i]==11) 
  {Sept2021$CLASE[i] <- 2} # Clase Media
  
  else if(Sept2021$CLASE[i]==4 | Sept2021$CLASE[i]==5 | 
          Sept2021$CLASE[i]==6 | Sept2021$CLASE[i]==7 | 
          Sept2021$CLASE[i]==8 | Sept2021$CLASE[i]==9 | 
          Sept2021$CLASE[i]==12) {Sept2021$CLASE[i] <- 1} # Clase Baja
  
  else if(Sept2021$CLASE[i]==10 | Sept2021$CLASE[i]==96 | 
          Sept2021$CLASE[i]==97 | Sept2021$CLASE[i]==98 | 
          Sept2021$CLASE[i]==99 ) {Sept2021$CLASE[i] <- -1} # NS/NC
}
# table(Sept2021$CLASE); table(Sept2021$CLASE2) # OK! Ha funcionado bien


# Variables exógenas que miden el grado de preocupación
# ------------------------------------------------------------------------------
# PREOC
# Sept2021$PREOC2 <- Sept2021$PREOC
for(i in 1:length(Sept2021$PREOC)){
  if(Sept2021$PREOC[i]==1  | Sept2021$PREOC[i]==2) 
  {Sept2021$PREOC[i] <- 3} # Mucho y bastante
  
  else if(Sept2021$PREOC[i]==3) {Sept2021$PREOC[i] <- 2} # Regular
  
  else if(Sept2021$PREOC[i]==4 | Sept2021$PREOC[i]==5) 
  {Sept2021$PREOC[i] <- 1} # Poco o nada
  
  else if(Sept2021$PREOC[i]==8 | Sept2021$PREOC[i]==9) 
  {Sept2021$PREOC[i] <- -1} # NS/NC
}
# table(Sept2021$PREOC); table(Sept2021$PREOC2) # OK! Ha funcionado bien

# OPT
# Sept2021$OPT2 <- Sept2021$OPT
for(i in 1:length(Sept2021$OPT)){
  if(Sept2021$OPT[i]==1) {Sept2021$OPT[i] <- 0} # Lo peor ha pasado ya
  else if(Sept2021$OPT[i]==2 | Sept2021$OPT[i]==3) 
  {Sept2021$OPT[i] <- 1} # Seguimos en lo peor o está por llegar
  else if(Sept2021$OPT[i]==8 | Sept2021$OPT[i]==9) 
  {Sept2021$OPT[i] <- -1} # NS/NC
}
# table(Sept2021$OPT); table(Sept2021$OPT2) # OK! Ha funcionado bien

# COMPGEN
# Sept2021$COMPGEN2 <- Sept2021$COMPGEN
for(i in 1:length(Sept2021$COMPGEN)){
  if(Sept2021$COMPGEN[i]==1) {Sept2021$COMPGEN[i] <- 0} 
  # Cree la mayoría está reaccionando con civismo y solidaridad
  else if(Sept2021$COMPGEN[i]==2) {Sept2021$COMPGEN[i] <- 1} 
  # Mayoría poco cívica e indisciplinada
  else if(Sept2021$COMPGEN[i]==8 | Sept2021$COMPGEN[i]==9) 
  {Sept2021$COMPGEN[i] <- -1} # NS/NC
}
# table(Sept2021$COMPGEN); table(Sept2021$COMPGEN2) # OK! Ha funcionado bien

# AFEC_P
# Sept2021$AFEC_P2 <- Sept2021$AFEC_P
for(i in 1:length(Sept2021$AFEC_P)){
  if(Sept2021$AFEC_P[i]==1 | Sept2021$AFEC_P[i]==2) 
  {Sept2021$AFEC_P[i] <- 1} # La pandemia ha afectado a su vida personal
  else if(Sept2021$AFEC_P[i]==3 | Sept2021$AFEC_P[i]==4 | 
          Sept2021$AFEC_P[i]==5) {Sept2021$AFEC_P[i] <- 0} 
  # La pandemia no ha afectado a su vida personal
  else if(Sept2021$AFEC_P[i]==8 | Sept2021$AFEC_P[i]==9) 
  {Sept2021$AFEC_P[i] <- -1} # NS/NC
}
# table(Sept2021$AFEC_P); table(Sept2021$AFEC_P2) # OK! Ha funcionado bien
# AF_ANIMICO (cat 1, 3 y 7), AF_SALUD (cat 5), AF_ECON (cat 2, 6 y 9), 
# AF_LIBERTAD (cat 4, 8 y 10), AF_OTROS(cat 96)

# Convierto los NA en 0, no menciona
for (i in 1:length(Sept2021$P10A_1)){
  if(is.na(Sept2021$P10A_1[i])) {Sept2021$P10A_1[i] <- 0}
  if(is.na(Sept2021$P10A_2[i])) {Sept2021$P10A_2[i] <- 0}
  if(is.na(Sept2021$P10A_3[i])) {Sept2021$P10A_3[i] <- 0}
  if(is.na(Sept2021$P10A_4[i])) {Sept2021$P10A_4[i] <- 0}
  if(is.na(Sept2021$P10A_5[i])) {Sept2021$P10A_5[i] <- 0}
  if(is.na(Sept2021$P10A_6[i])) {Sept2021$P10A_6[i] <- 0}
  if(is.na(Sept2021$P10A_7[i])) {Sept2021$P10A_7[i] <- 0}
  if(is.na(Sept2021$P10A_8[i])) {Sept2021$P10A_8[i] <- 0}
  if(is.na(Sept2021$P10A_9[i])) {Sept2021$P10A_9[i] <- 0}
  if(is.na(Sept2021$P10A_10[i])){Sept2021$P10A_10[i] <- 0}
  if(is.na(Sept2021$P10A_96[i])){Sept2021$P10A_96[i] <- 0}
  if(is.na(Sept2021$P10A_98[i])){Sept2021$P10A_98[i] <- 0}
  if(is.na(Sept2021$P10A_99[i])){Sept2021$P10A_99[i] <- 0}
}

# Inicializo en 0 (no ha afectado o no menciona)
Sept2021$AF_ANIMICO  <- rep(0, length(Sept2021$P10A_1))
Sept2021$AF_SALUD    <- rep(0, length(Sept2021$P10A_1))
Sept2021$AF_ECON     <- rep(0, length(Sept2021$P10A_1))
Sept2021$AF_LIBERTAD <- rep(0, length(Sept2021$P10A_1))
Sept2021$AF_OTROS    <- rep(0, length(Sept2021$P10A_1))

for (i in 1:length(Sept2021$P10A_1)){
  # AF_ANIMICO (1, 3 y 7)
  if(Sept2021$P10A_1[i]==1 | Sept2021$P10A_3[i]==1 | Sept2021$P10A_7[i]==1) 
  {Sept2021$AF_ANIMICO[i] <- 1}
  # AF_SALUD (5)
  if(Sept2021$P10A_5[i]==1) {Sept2021$AF_SALUD[i] <- 1}
  # AF_ECON (2, 6 y 9)
  if(Sept2021$P10A_2[i]==1 | Sept2021$P10A_6[i]==1 | Sept2021$P10A_9[i]==1) 
  {Sept2021$AF_ECON[i] <- 1}
  # AF_LIBERTAD (4, 8 y 10)
  if(Sept2021$P10A_4[i]==1 | Sept2021$P10A_8[i]==1 | Sept2021$P10A_10[i]==1) 
  {Sept2021$AF_LIBERTAD[i] <- 1}
  # AF_OTROS (96)
  if(Sept2021$P10A_96[i]==1) {Sept2021$AF_OTROS[i] <- 1}
}


# Variables exógenas enfermedad
# ------------------------------------------------------------------------------
# SINTOM
# Sept2021$SINTOM2 <- Sept2021$SINTOM
for(i in 1:length(Sept2021$SINTOM)){
  if(Sept2021$SINTOM[i]==3) {Sept2021$SINTOM[i] <- 1} # Ha tenido síntomas
  else if(Sept2021$SINTOM[i]==2) {Sept2021$SINTOM[i] <- 0} # No ha tenido síntomas
  else if(Sept2021$SINTOM[i]==9) {Sept2021$SINTOM[i] <- -1} # NS/NC
}
# table(Sept2021$SINTOM); table(Sept2021$SINTOM2) # OK! Ha funcionado bien

# DIAG i EVOL
# Sept2021$DIAG2 <- Sept2021$DIAG
# Sept2021$EVOL2 <- Sept2021$EVOL

for (i in 1:length(Sept2021$DIAG)){
  if(is.na(Sept2021$DIAG[i])){Sept2021$DIAG[i] <-0}
  else if(Sept2021$DIAG[i]==1){Sept2021$DIAG[i]<-1}
  else if(Sept2021$DIAG[i]==2){Sept2021$DIAG[i]<-0}
  else if(Sept2021$DIAG[i]==9){Sept2021$DIAG[i]<-0}
}

for (i in 1:length(Sept2021$EVOL)){
  if(is.na(Sept2021$EVOL[i])){Sept2021$EVOL[i]<-1} 
  else if(Sept2021$EVOL[i]==4){Sept2021$EVOL[i]<-1} 
  # No se ha contagiado o ha sido asintomático
  
  else if(Sept2021$EVOL[i]==1){Sept2021$EVOL[i]<-2} 
  # Diagnóstico favorable, con síntomas leves
  
  else if(Sept2021$EVOL[i]==2){Sept2021$EVOL[i]<-3} 
  # Diagnóstico favorable, síntomas importantes sin hospitalización
  
  else if(Sept2021$EVOL[i]==3){Sept2021$EVOL[i]<-4} 
  # Diagnóstico favorable, hospitalización
  
  else if(Sept2021$EVOL[i]==9){Sept2021$EVOL[i]<--1} # NS/NC
}

# table(Sept2021$DIAG); table(Sept2021$DIAG2) # OK! Ha funcionado bien
# table(Sept2021$EVOL); table(Sept2021$EVOL2) # OK! Ha funcionado bien


# Descargamos la base de datos arreglada
vars <- c("OBLIG", "PROB_COVID", "PERS_COVID", 
          "CCAA", "TAMUNI", "SEXO", "EDAD", "EDAD.c", "ESTADO_CIVIL", "EXTR",
          "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
          "PREOC", "OPT", "AFEC_P", "AF_ANIMICO", "AF_SALUD", "AF_ECON", 
          "AF_LIBERTAD", "AF_OTROS", "COMPGEN", 
          "SINTOM", "DIAG", "EVOL")

dd_neta <- as.data.frame(Sept2021[,vars])

# Descargamos en CSV (o xlsx)
# write.csv2(dd_neta, "Sept2021_neta.csv")





# Anexo 3.3: Pre-processing del barómetro de Enero de 2021
# ------------------------------------------------------------------------------
# Importación de la Base de Datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/2. Enero 2021")
Enero2021 <- read_delim("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/2. Enero 2021/Ene2021.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Nombre variables base original
names <- names(Enero2021)

# Seleccionamos las variables que se van a utilizar i cambiamos el nombre

var <- c("P14_1", "P15_1", 
         "CCAA", "TAMUNI", "SEXO", "EDAD", "ECIVIL", "P0", "ESCIDEOL", 
         "ESTUDIOS", "RELIGION", "SITLAB", "P13", "CLASESOCIAL", 
         "P1", "P3", "P8", "P10A_1", "P10A_2", "P10A_3", "P10A_4", "P10A_5", 
         "P10A_6", "P10A_7", "P10A_8", "P10A_9", "P10A_10","P10A_97", "P10A_98","P10A_99", 
         "P7", 
         "P4", "P4E", "P4F")


Enero2021 <- Enero2021[,var]

names(Enero2021) <- c("PROB_COVID1", "PERS_COVID1", 
                     "CCAA", "TAMUNI", "SEXO", "EDAD", "ESTADO_CIVIL", "EXTR", 
                     "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
                     "PREOC", "OPT", "AFEC_P", "P10A_1", "P10A_2", "P10A_3", 
                     "P10A_4", "P10A_5", "P10A_6", "P10A_7", "P10A_8", "P10A_9",
                     "P10A_10", "P10A_97", "P10A_98","P10A_99", "COMPGEN", 
                     "SINTOM", "DIAG", "EVOL")


# Variables endógenas
# ------------------------------------------------------------------------------
# PROB_COVID: Primer problema de españa es el COVID o alguno relacionado con él 
# (categorias 53, 59, 60, 61, 62, 63, 64, 67, 68 y 69)
# PERS_COVID: Primer problema personal  es el COVID o alguno relacionado con él

# Enero2021$PROB_COVID1.2 <- Enero2021$PROB_COVID1
# Enero2021$PERS_COVID1.2 <- Enero2021$PERS_COVID1
Enero2021$PROB_COVID <- rep(0, length(Enero2021$PROB_COVID1))
Enero2021$PERS_COVID <- rep(0, length(Enero2021$PROB_COVID1))

for(i in 1:length(Enero2021$PROB_COVID)){
  if(Enero2021$PROB_COVID1[i]=="53" | Enero2021$PROB_COVID1[i]=="59" | 
     Enero2021$PROB_COVID1[i]=="60" | Enero2021$PROB_COVID1[i]=="61" | 
     Enero2021$PROB_COVID1[i]=="62" | Enero2021$PROB_COVID1[i]=="63" |
     Enero2021$PROB_COVID1[i]=="64" | Enero2021$PROB_COVID1[i]=="67" | 
     Enero2021$PROB_COVID1[i]=="68" | Enero2021$PROB_COVID1[i]=="69") 
    
  { Enero2021$PROB_COVID[i]<- 1 }
  
  if(Enero2021$PERS_COVID1[i]=="53" | Enero2021$PERS_COVID1[i]=="59" | 
     Enero2021$PERS_COVID1[i]=="60" | Enero2021$PERS_COVID1[i]=="61" | 
     Enero2021$PERS_COVID1[i]=="62" | Enero2021$PERS_COVID1[i]=="63" |
     Enero2021$PERS_COVID1[i]=="64" | Enero2021$PERS_COVID1[i]=="67" | 
     Enero2021$PERS_COVID1[i]=="68" | Enero2021$PERS_COVID1[i]=="69") 
    
  { Enero2021$PERS_COVID[i]<- 1 }
  
}
# table(Enero2021$PROB_COVID1.2); table(Enero2021$PERS_COVID1.2)
# table(Enero2021$PROB_COVID); table(Enero2021$PERS_COVID)
# OK! Ha funcionado bien


# Variables exógenas de tipo socioeconómico
# ------------------------------------------------------------------------------
# SEXO
Enero2021$SEXO <- Enero2021$SEXO - 1 # 0: Hombre
                                     # 1: Mujer

# EDAD
Enero2021$EDAD.c <- c()
for(i in 1:length(Enero2021$EDAD)){
  if(Enero2021$EDAD[i]<=24){Enero2021$EDAD.c[i] <- 1}
  if(Enero2021$EDAD[i]>24 && Enero2021$EDAD[i]<=34){Enero2021$EDAD.c[i] <- 2}
  if(Enero2021$EDAD[i]>34 && Enero2021$EDAD[i]<=44){Enero2021$EDAD.c[i] <- 3}
  if(Enero2021$EDAD[i]>44 && Enero2021$EDAD[i]<=54){Enero2021$EDAD.c[i] <- 4}
  if(Enero2021$EDAD[i]>54 && Enero2021$EDAD[i]<=64){Enero2021$EDAD.c[i] <- 5}
  if(Enero2021$EDAD[i]>=65){Enero2021$EDAD.c[i] <- 6}
}
# table(Enero2021$EDAD.c); table(Enero2021$EDAD) # OK! Ha funcionado bien

# ESTADO_CIVIL
# Enero2021$ESTADO_CIVIL2 <- Enero2021$ESTADO_CIVIL
for(i in 1:length(Enero2021$ESTADO_CIVIL)){
  if(Enero2021$ESTADO_CIVIL[i]==5){Enero2021$ESTADO_CIVIL[i] <- 4}
  if(Enero2021$ESTADO_CIVIL[i]==9){Enero2021$ESTADO_CIVIL[i] <- -1}
}
# table(Enero2021$ESTADO_CIVIL); table(Enero2021$ESTADO_CIVIL2) 
# OK! Ha funcionado bien

# ESTR
# Enero2021$EXTR2 <- Enero2021$EXTR
for(i in 1:length(Enero2021$EXTR)){
  if(Enero2021$EXTR[i]==1){Enero2021$EXTR[i] <- 0} # Españoles
  if(Enero2021$EXTR[i]==2 | Enero2021$EXTR[i]==3) {Enero2021$EXTR[i] <- 1}
  # Extranjeros o doble nacionalidad
}
# table(Enero2021$EXTR); table(Enero2021$EXTR2) 
# OK! Ha funcionado bien

# ORIENT
# Enero2021$ORIENT2 <- Enero2021$ORIENT
for(i in 1:length(Enero2021$ORIENT)){
  if(Enero2021$ORIENT[i]==1  | Enero2021$ORIENT[i]==2) {Enero2021$ORIENT[i] <- 1}
  if(Enero2021$ORIENT[i]==3  | Enero2021$ORIENT[i]==4) {Enero2021$ORIENT[i] <- 2}
  if(Enero2021$ORIENT[i]==5  | Enero2021$ORIENT[i]==6) {Enero2021$ORIENT[i] <- 3}
  if(Enero2021$ORIENT[i]==7  | Enero2021$ORIENT[i]==8) {Enero2021$ORIENT[i] <- 4}
  if(Enero2021$ORIENT[i]==9  | Enero2021$ORIENT[i]==10){Enero2021$ORIENT[i] <- 5}
  if(Enero2021$ORIENT[i]==98 | Enero2021$ORIENT[i]==99){Enero2021$ORIENT[i] <- -1}
}
# table(Enero2021$ORIENT); table(Enero2021$ORIENT2) # OK! Ha funcionado bien

# ESTUD
# Enero2021$ESTUD2 <- Enero2021$ESTUD
for(i in 1:length(Enero2021$ESTUD)){
  if(Enero2021$ESTUD[i]==1  | Enero2021$ESTUD[i]==2) {Enero2021$ESTUD[i] <- 1}
  if(Enero2021$ESTUD[i]==3  | Enero2021$ESTUD[i]==4 | Enero2021$ESTUD[i]==5  ) 
  {Enero2021$ESTUD[i] <- 2}
  if(Enero2021$ESTUD[i]==6) {Enero2021$ESTUD[i] <- 3}
  if(Enero2021$ESTUD[i]==7  | Enero2021$ESTUD[i]==9) {Enero2021$ESTUD[i] <- -1}
}
# table(Enero2021$ESTUD); table(Enero2021$ESTUD2) # OK! Ha funcionado bien

# RELIG
# Enero2021$RELIG2 <- Enero2021$RELIG
for(i in 1:length(Enero2021$RELIG)){
  if(Enero2021$RELIG[i]==1  | Enero2021$RELIG[i]==2) {Enero2021$RELIG[i] <- 1} # Católico
  if(Enero2021$RELIG[i]==3) {Enero2021$RELIG[i] <- 2} # Creyente de otra religión
  if(Enero2021$RELIG[i]==4  | Enero2021$RELIG[i]==5 | 
     Enero2021$RELIG[i]==6 ) {Enero2021$RELIG[i] <- 3} # Agnóstico, indiferente, ateo/a
  if(Enero2021$RELIG[i]==9) {Enero2021$RELIG[i] <- -1} # NS/NC
}
# table(Enero2021$RELIG); table(Enero2021$RELIG2) # OK! Ha funcionado bien

# SIT_LAB
# Enero2021$SIT_LAB2 <- Enero2021$SIT_LAB
for(i in 1:length(Enero2021$SIT_LAB)){
  if(Enero2021$SIT_LAB[i]==1) {Enero2021$SIT_LAB[i] <- 1} # Trabaja
  if(Enero2021$SIT_LAB[i]==2  | Enero2021$SIT_LAB[i]==3) 
  {Enero2021$SIT_LAB[i] <- 2} # Jubilado o Pensionista
  if(Enero2021$SIT_LAB[i]==4  | Enero2021$SIT_LAB[i]==5) 
  {Enero2021$SIT_LAB[i] <- 3} # Parado
  if(Enero2021$SIT_LAB[i]==6) {Enero2021$SIT_LAB[i] <- 4} 
  # Estudiante
  if(Enero2021$SIT_LAB[i]==7  | Enero2021$SIT_LAB[i]==8) 
  {Enero2021$SIT_LAB[i] <- 5} # Otros
  if(Enero2021$SIT_LAB[i]==9) {Enero2021$SIT_LAB[i] <- -1} # NS/NC
}
# table(Enero2021$SIT_LAB); table(Enero2021$SIT_LAB2) # OK! Ha funcionado bien

# SIT_ECON
# Enero2021$SIT_ECON2 <- Enero2021$SIT_ECON
for(i in 1:length(Enero2021$SIT_ECON)){
  if(Enero2021$SIT_ECON[i]==1  | Enero2021$SIT_ECON[i]==2) 
  {Enero2021$SIT_ECON[i] <- 3} # Buena o muy buena
  else if(Enero2021$SIT_ECON[i]==3) {Enero2021$SIT_ECON[i] <- 2} # Regular
  else if(Enero2021$SIT_ECON[i]==4  | Enero2021$SIT_ECON[i]==5) 
  {Enero2021$SIT_ECON[i] <- 1} # Mala o muy mala
  else if(Enero2021$SIT_ECON[i]==8  | Enero2021$SIT_ECON[i]==9) 
  {Enero2021$SIT_ECON[i] <- -1} # NS/NC
}
# table(Enero2021$SIT_ECON); table(Enero2021$SIT_ECON2) # OK! Ha funcionado bien

# CLASE
# Enero2021$CLASE2 <- Enero2021$CLASE
for(i in 1:length(Enero2021$CLASE)){
  
  if(Enero2021$CLASE[i]==1  | Enero2021$CLASE[i]==2) 
  {Enero2021$CLASE[i] <- 3} # Clase alta y media alta
  
  else if(Enero2021$CLASE[i]==3 | Enero2021$CLASE[i]==11) 
  {Enero2021$CLASE[i] <- 2} # Clase Media
  
  else if(Enero2021$CLASE[i]==4 | Enero2021$CLASE[i]==5 | 
          Enero2021$CLASE[i]==6 | Enero2021$CLASE[i]==7 | 
          Enero2021$CLASE[i]==8 | Enero2021$CLASE[i]==9 | 
          Enero2021$CLASE[i]==12) {Enero2021$CLASE[i] <- 1} # Clase Baja
  
  else if(Enero2021$CLASE[i]==10 | Enero2021$CLASE[i]==96 | 
          Enero2021$CLASE[i]==97 | Enero2021$CLASE[i]==98 | 
          Enero2021$CLASE[i]==99 ) {Enero2021$CLASE[i] <- -1} # NS/NC
}
# table(Enero2021$CLASE); table(Enero2021$CLASE2) # OK! Ha funcionado bien


# Variables exógenas que miden el grado de preocupación
# ------------------------------------------------------------------------------
# PREOC
# Enero2021$PREOC2 <- Enero2021$PREOC
for(i in 1:length(Enero2021$PREOC)){
  if(Enero2021$PREOC[i]==1  | Enero2021$PREOC[i]==2) 
  {Enero2021$PREOC[i] <- 3} # Mucho y bastante
  
  else if(Enero2021$PREOC[i]==3) {Enero2021$PREOC[i] <- 2} # Regular
  
  else if(Enero2021$PREOC[i]==4 | Enero2021$PREOC[i]==5) 
  {Enero2021$PREOC[i] <- 1} # Poco o nada
  
  else if(Enero2021$PREOC[i]==8 | Enero2021$PREOC[i]==9) 
  {Enero2021$PREOC[i] <- -1} # NS/NC
}
# table(Enero2021$PREOC); table(Enero2021$PREOC2) # OK! Ha funcionado bien

# OPT
# Enero2021$OPT2 <- Enero2021$OPT
for(i in 1:length(Enero2021$OPT)){
  if(Enero2021$OPT[i]==1) {Enero2021$OPT[i] <- 0} # Lo peor ha pasado ya
  else if(Enero2021$OPT[i]==2 | Enero2021$OPT[i]==3) 
  {Enero2021$OPT[i] <- 1} # Seguimos en lo peor o está por llegar
  else if(Enero2021$OPT[i]==8 | Enero2021$OPT[i]==9) 
  {Enero2021$OPT[i] <- -1} # NS/NC
}
# table(Enero2021$OPT); table(Enero2021$OPT2) # OK! Ha funcionado bien

# COMPGEN
# Enero2021$COMPGEN2 <- Enero2021$COMPGEN
for(i in 1:length(Enero2021$COMPGEN)){
  if(Enero2021$COMPGEN[i]==1) {Enero2021$COMPGEN[i] <- 0} 
  # Cree la mayoría está reaccionando con civismo y solidaridad
  else if(Enero2021$COMPGEN[i]==2) {Enero2021$COMPGEN[i] <- 1} 
  # Mayoría poco cívica e indisciplinada
  else if(Enero2021$COMPGEN[i]==8 | Enero2021$COMPGEN[i]==9) 
  {Enero2021$COMPGEN[i] <- -1} # NS/NC
}
# table(Enero2021$COMPGEN); table(Enero2021$COMPGEN2) # OK! Ha funcionado bien

# AFEC_P
# Enero2021$AFEC_P2 <- Enero2021$AFEC_P
for(i in 1:length(Enero2021$AFEC_P)){
  if(Enero2021$AFEC_P[i]==1 | Enero2021$AFEC_P[i]==2) 
  {Enero2021$AFEC_P[i] <- 1} # La pandemia ha afectado a su vida personal
  else if(Enero2021$AFEC_P[i]==3 | Enero2021$AFEC_P[i]==4 | 
          Enero2021$AFEC_P[i]==6) {Enero2021$AFEC_P[i] <- 0} 
  # La pandemia no ha afectado a su vida personal
  else if(Enero2021$AFEC_P[i]==8 | Enero2021$AFEC_P[i]==9) 
  {Enero2021$AFEC_P[i] <- -1} # NS/NC
}
# table(Enero2021$AFEC_P); table(Enero2021$AFEC_P2) # OK! Ha funcionado bien

# AF_ANIMICO (cat 1, 3 y 7), AF_SALUD (cat 5), AF_ECON (cat 2, 6 y 9), 
# AF_LIBERTAD (cat 4, 8 y 10), AF_OTROS(cat 96)

# Convierto los NA en 0, no menciona
for (i in 1:length(Enero2021$P10A_1)){
  if(is.na(Enero2021$P10A_1[i])) {Enero2021$P10A_1[i] <- 0}
  if(is.na(Enero2021$P10A_2[i])) {Enero2021$P10A_2[i] <- 0}
  if(is.na(Enero2021$P10A_3[i])) {Enero2021$P10A_3[i] <- 0}
  if(is.na(Enero2021$P10A_4[i])) {Enero2021$P10A_4[i] <- 0}
  if(is.na(Enero2021$P10A_5[i])) {Enero2021$P10A_5[i] <- 0}
  if(is.na(Enero2021$P10A_6[i])) {Enero2021$P10A_6[i] <- 0}
  if(is.na(Enero2021$P10A_7[i])) {Enero2021$P10A_7[i] <- 0}
  if(is.na(Enero2021$P10A_8[i])) {Enero2021$P10A_8[i] <- 0}
  if(is.na(Enero2021$P10A_9[i])) {Enero2021$P10A_9[i] <- 0}
  if(is.na(Enero2021$P10A_10[i])){Enero2021$P10A_10[i] <- 0}
  if(is.na(Enero2021$P10A_97[i])){Enero2021$P10A_97[i] <- 0}
  if(is.na(Enero2021$P10A_98[i])){Enero2021$P10A_98[i] <- 0}
  if(is.na(Enero2021$P10A_99[i])){Enero2021$P10A_99[i] <- 0}
}

# Inicializo en 0 (no ha afectado o no menciona)
Enero2021$AF_ANIMICO  <- rep(0, length(Enero2021$P10A_1))
Enero2021$AF_SALUD    <- rep(0, length(Enero2021$P10A_1))
Enero2021$AF_ECON     <- rep(0, length(Enero2021$P10A_1))
Enero2021$AF_LIBERTAD <- rep(0, length(Enero2021$P10A_1))
Enero2021$AF_OTROS    <- rep(0, length(Enero2021$P10A_1))

for (i in 1:length(Enero2021$P10A_1)){
  # AF_ANIMICO (1, 3 y 7)
  if(Enero2021$P10A_1[i]==1 | Enero2021$P10A_3[i]==1 | Enero2021$P10A_7[i]==1) 
  {Enero2021$AF_ANIMICO[i] <- 1}
  # AF_SALUD (5)
  if(Enero2021$P10A_5[i]==1) {Enero2021$AF_SALUD[i] <- 1}
  # AF_ECON (2, 6 y 9)
  if(Enero2021$P10A_2[i]==1 | Enero2021$P10A_6[i]==1 | Enero2021$P10A_9[i]==1) 
  {Enero2021$AF_ECON[i] <- 1}
  # AF_LIBERTAD (4, 8 y 10)
  if(Enero2021$P10A_4[i]==1 | Enero2021$P10A_8[i]==1 | Enero2021$P10A_10[i]==1) 
  {Enero2021$AF_LIBERTAD[i] <- 1}
  # AF_OTROS (96)
  if(Enero2021$P10A_97[i]==1) {Enero2021$AF_OTROS[i] <- 1}
}


# Variables exógenas enfermedad
# ------------------------------------------------------------------------------
# SINTOM
# Enero2021$SINTOM2 <- Enero2021$SINTOM
for(i in 1:length(Enero2021$SINTOM)){
  if(Enero2021$SINTOM[i]==1) {Enero2021$SINTOM[i] <- 1} # Ha tenido síntomas
  else if(Enero2021$SINTOM[i]==2) {Enero2021$SINTOM[i] <- 0} # No ha tenido síntomas
  else if(Enero2021$SINTOM[i]==9) {Enero2021$SINTOM[i] <- -1} # NS/NC
}
# table(Enero2021$SINTOM); table(Enero2021$SINTOM2) # OK! Ha funcionado bien

# DIAG i EVOL
# Enero2021$DIAG2 <- Enero2021$DIAG
# Enero2021$EVOL2 <- Enero2021$EVOL

for (i in 1:length(Enero2021$DIAG)){
  if(is.na(Enero2021$DIAG[i])){Enero2021$DIAG[i] <-0}
  else if(Enero2021$DIAG[i]==1){Enero2021$DIAG[i]<-1}
  else if(Enero2021$DIAG[i]==2){Enero2021$DIAG[i]<-0}
  else if(Enero2021$DIAG[i]==9){Enero2021$DIAG[i]<-0}
}

for (i in 1:length(Enero2021$EVOL)){
  if(is.na(Enero2021$EVOL[i])){Enero2021$EVOL[i]<-1} 
  # No se ha contagiado o ha sido asintomático
  
  else if(Enero2021$EVOL[i]==1){Enero2021$EVOL[i]<-2} 
  # Diagnóstico favorable, con síntomas leves
  
  else if(Enero2021$EVOL[i]==2){Enero2021$EVOL[i]<-3} 
  # Diagnóstico favorable, síntomas importantes sin hospitalización
  
  else if(Enero2021$EVOL[i]==3){Enero2021$EVOL[i]<-4} 
  # Diagnóstico favorable, hospitalización
  
  else if(Enero2021$EVOL[i]==9){Enero2021$EVOL[i]<--1} # NS/NC
}

# table(Enero2021$DIAG); table(Enero2021$DIAG2) # OK! Ha funcionado bien
# table(Enero2021$EVOL); table(Enero2021$EVOL2) # OK! Ha funcionado bien


# Descargamos la base de datos arreglada
vars <- c("PROB_COVID", "PERS_COVID", 
          "CCAA", "TAMUNI", "SEXO", "EDAD", "EDAD.c", "ESTADO_CIVIL", "EXTR",
          "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
          "PREOC", "OPT", "AFEC_P", "AF_ANIMICO", "AF_SALUD", "AF_ECON", 
          "AF_LIBERTAD", "AF_OTROS", "COMPGEN", 
          "SINTOM", "DIAG", "EVOL")

dd_neta <- as.data.frame(Enero2021[,vars])

# Descargamos en CSV (o xlsx)
# write.csv2(dd_neta, "Enero2021_neta.csv")




# Anexo 3.4: Pre-processing del barómetro de Abril de 2020
# ------------------------------------------------------------------------------
# Importación de la Base de Datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/1. Abril 2020")
Abril2020 <- read_delim("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/1. Abril 2020/Abr2020.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Nombre variables base original
names <- names(Abril2020)

# Seleccionamos las variables que se van a utilizar i cambiamos el nombre

var <- c("P21_1", "P22_1", 
         "CCAA", "TAMUNI", "SEXO", "EDAD", "ESTADOCIVIL", "NACIONALIDAD", 
         "ESCIDEOL", "ESTUDIOS", "RELIGION", "SITLAB", "P19", "CLASESOCIAL", 
         "P1", "P9", "P11")


Abril2020 <- Abril2020[,var]

names(Abril2020) <- c("PROB_COVID1", "PERS_COVID1", 
                      "CCAA", "TAMUNI", "SEXO", "EDAD", "ESTADO_CIVIL", "EXTR", 
                      "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
                      "PREOC", "OPT", "COMPGEN")


# Variables endógenas
# ------------------------------------------------------------------------------
# PROB_COVID: Primer problema de españa es el COVID o alguno relacionado con él 
# (categorias 53, 69, 70, 73, 93 y 94)
# PERS_COVID: Primer problema personal  es el COVID o alguno relacionado con él

# Abril2020$PROB_COVID1.2 <- Abril2020$PROB_COVID1
# Abril2020$PERS_COVID1.2 <- Abril2020$PERS_COVID1
Abril2020$PROB_COVID <- rep(0, length(Abril2020$PROB_COVID1))
Abril2020$PERS_COVID <- rep(0, length(Abril2020$PROB_COVID1))

for(i in 1:length(Abril2020$PROB_COVID)){
  if(Abril2020$PROB_COVID1[i]=="53" | Abril2020$PROB_COVID1[i]=="69" | 
     Abril2020$PROB_COVID1[i]=="70" | Abril2020$PROB_COVID1[i]=="73" | 
     Abril2020$PROB_COVID1[i]=="93" | Abril2020$PROB_COVID1[i]=="94" ) 
    
  { Abril2020$PROB_COVID[i]<- 1 }
  
  if(Abril2020$PERS_COVID1[i]=="53" | Abril2020$PERS_COVID1[i]=="69" | 
     Abril2020$PERS_COVID1[i]=="70" | Abril2020$PERS_COVID1[i]=="73" | 
     Abril2020$PERS_COVID1[i]=="93" | Abril2020$PERS_COVID1[i]=="94" ) 
    
  { Abril2020$PERS_COVID[i]<- 1 }
  
}
# table(Abril2020$PROB_COVID1.2); table(Abril2020$PERS_COVID1.2)
# table(Abril2020$PROB_COVID); table(Abril2020$PERS_COVID)
# OK! Ha funcionado bien


# Variables exógenas de tipo socioeconómico
# ------------------------------------------------------------------------------
# SEXO
Abril2020$SEXO <- Abril2020$SEXO - 1 # 0: Hombre
# 1: Mujer

# EDAD
Abril2020$EDAD.c <- c()
for(i in 1:length(Abril2020$EDAD)){
  if(Abril2020$EDAD[i]<=24){Abril2020$EDAD.c[i] <- 1}
  if(Abril2020$EDAD[i]>24 && Abril2020$EDAD[i]<=34){Abril2020$EDAD.c[i] <- 2}
  if(Abril2020$EDAD[i]>34 && Abril2020$EDAD[i]<=44){Abril2020$EDAD.c[i] <- 3}
  if(Abril2020$EDAD[i]>44 && Abril2020$EDAD[i]<=54){Abril2020$EDAD.c[i] <- 4}
  if(Abril2020$EDAD[i]>54 && Abril2020$EDAD[i]<=64){Abril2020$EDAD.c[i] <- 5}
  if(Abril2020$EDAD[i]>=65){Abril2020$EDAD.c[i] <- 6}
}
# table(Abril2020$EDAD.c); table(Abril2020$EDAD) # OK! Ha funcionado bien

# ESTADO_CIVIL
# Abril2020$ESTADO_CIVIL2 <- Abril2020$ESTADO_CIVIL
for(i in 1:length(Abril2020$ESTADO_CIVIL)){
  if(Abril2020$ESTADO_CIVIL[i]==5){Abril2020$ESTADO_CIVIL[i] <- 4}
  if(Abril2020$ESTADO_CIVIL[i]==9){Abril2020$ESTADO_CIVIL[i] <- -1}
}
# table(Abril2020$ESTADO_CIVIL); table(Abril2020$ESTADO_CIVIL2) 
# OK! Ha funcionado bien

# ESTR
# Abril2020$EXTR2 <- Abril2020$EXTR
for(i in 1:length(Abril2020$EXTR)){
  if(Abril2020$EXTR[i]==1){Abril2020$EXTR[i] <- 0} # Españoles
  if(Abril2020$EXTR[i]==2 | Abril2020$EXTR[i]==3) {Abril2020$EXTR[i] <- 1}
  # Extranjeros o doble nacionalidad
}
# table(Abril2020$EXTR); table(Abril2020$EXTR2) 
# OK! Ha funcionado bien

# ORIENT
# Abril2020$ORIENT2 <- Abril2020$ORIENT
for(i in 1:length(Abril2020$ORIENT)){
  if(Abril2020$ORIENT[i]==1  | Abril2020$ORIENT[i]==2) {Abril2020$ORIENT[i] <- 1}
  if(Abril2020$ORIENT[i]==3  | Abril2020$ORIENT[i]==4) {Abril2020$ORIENT[i] <- 2}
  if(Abril2020$ORIENT[i]==5  | Abril2020$ORIENT[i]==6) {Abril2020$ORIENT[i] <- 3}
  if(Abril2020$ORIENT[i]==7  | Abril2020$ORIENT[i]==8) {Abril2020$ORIENT[i] <- 4}
  if(Abril2020$ORIENT[i]==9  | Abril2020$ORIENT[i]==10){Abril2020$ORIENT[i] <- 5}
  if(Abril2020$ORIENT[i]==98 | Abril2020$ORIENT[i]==99){Abril2020$ORIENT[i] <- -1}
}
# table(Abril2020$ORIENT); table(Abril2020$ORIENT2) # OK! Ha funcionado bien

# ESTUD
# Abril2020$ESTUD2 <- Abril2020$ESTUD
for(i in 1:length(Abril2020$ESTUD)){
  if(Abril2020$ESTUD[i]==1  | Abril2020$ESTUD[i]==2) {Abril2020$ESTUD[i] <- 1}
  if(Abril2020$ESTUD[i]==3  | Abril2020$ESTUD[i]==4 | Abril2020$ESTUD[i]==5  ) 
  {Abril2020$ESTUD[i] <- 2}
  if(Abril2020$ESTUD[i]==6) {Abril2020$ESTUD[i] <- 3}
  if(Abril2020$ESTUD[i]==7  | Abril2020$ESTUD[i]==9) {Abril2020$ESTUD[i] <- -1}
}
# table(Abril2020$ESTUD); table(Abril2020$ESTUD2) # OK! Ha funcionado bien

# RELIG
# Abril2020$RELIG2 <- Abril2020$RELIG
for(i in 1:length(Abril2020$RELIG)){
  if(Abril2020$RELIG[i]==1  | Abril2020$RELIG[i]==2) {Abril2020$RELIG[i] <- 1} # Católico
  if(Abril2020$RELIG[i]==3) {Abril2020$RELIG[i] <- 2} # Creyente de otra religión
  if(Abril2020$RELIG[i]==4  | Abril2020$RELIG[i]==5 | 
     Abril2020$RELIG[i]==6 ) {Abril2020$RELIG[i] <- 3} # Agnóstico, indiferente, ateo/a
  if(Abril2020$RELIG[i]==9) {Abril2020$RELIG[i] <- -1} # NS/NC
}
# table(Abril2020$RELIG); table(Abril2020$RELIG2) # OK! Ha funcionado bien

# SIT_LAB
# Abril2020$SIT_LAB2 <- Abril2020$SIT_LAB
for(i in 1:length(Abril2020$SIT_LAB)){
  if(Abril2020$SIT_LAB[i]==1) {Abril2020$SIT_LAB[i] <- 1} # Trabaja
  if(Abril2020$SIT_LAB[i]==2  | Abril2020$SIT_LAB[i]==3) 
  {Abril2020$SIT_LAB[i] <- 2} # Jubilado o Pensionista
  if(Abril2020$SIT_LAB[i]==4  | Abril2020$SIT_LAB[i]==5) 
  {Abril2020$SIT_LAB[i] <- 3} # Parado
  if(Abril2020$SIT_LAB[i]==6) {Abril2020$SIT_LAB[i] <- 4} 
  # Estudiante
  if(Abril2020$SIT_LAB[i]==7  | Abril2020$SIT_LAB[i]==8) 
  {Abril2020$SIT_LAB[i] <- 5} # Otros
  if(Abril2020$SIT_LAB[i]==9) {Abril2020$SIT_LAB[i] <- -1} # NS/NC
}
# table(Abril2020$SIT_LAB); table(Abril2020$SIT_LAB2) # OK! Ha funcionado bien

# SIT_ECON
# Abril2020$SIT_ECON2 <- Abril2020$SIT_ECON
for(i in 1:length(Abril2020$SIT_ECON)){
  if(Abril2020$SIT_ECON[i]==1  | Abril2020$SIT_ECON[i]==2) 
  {Abril2020$SIT_ECON[i] <- 3} # Buena o muy buena
  else if(Abril2020$SIT_ECON[i]==3) {Abril2020$SIT_ECON[i] <- 2} # Regular
  else if(Abril2020$SIT_ECON[i]==4  | Abril2020$SIT_ECON[i]==5) 
  {Abril2020$SIT_ECON[i] <- 1} # Mala o muy mala
  else if(Abril2020$SIT_ECON[i]==8  | Abril2020$SIT_ECON[i]==9) 
  {Abril2020$SIT_ECON[i] <- -1} # NS/NC
}
# table(Abril2020$SIT_ECON); table(Abril2020$SIT_ECON2) # OK! Ha funcionado bien

# CLASE
# Abril2020$CLASE2 <- Abril2020$CLASE
for(i in 1:length(Abril2020$CLASE)){
  
  if(Abril2020$CLASE[i]==1  | Abril2020$CLASE[i]==2) 
  {Abril2020$CLASE[i] <- 3} # Clase alta y media alta
  
  else if(Abril2020$CLASE[i]==3 | Abril2020$CLASE[i]==11) 
  {Abril2020$CLASE[i] <- 2} # Clase Media
  
  else if(Abril2020$CLASE[i]==4 | Abril2020$CLASE[i]==5 | 
          Abril2020$CLASE[i]==6 | Abril2020$CLASE[i]==7 | 
          Abril2020$CLASE[i]==8 | Abril2020$CLASE[i]==9 | 
          Abril2020$CLASE[i]==12) {Abril2020$CLASE[i] <- 1} # Clase Baja
  
  else if(Abril2020$CLASE[i]==10 | Abril2020$CLASE[i]==96 | 
          Abril2020$CLASE[i]==97 | Abril2020$CLASE[i]==98 | 
          Abril2020$CLASE[i]==99 ) {Abril2020$CLASE[i] <- -1} # NS/NC
}
# table(Abril2020$CLASE); table(Abril2020$CLASE2) # OK! Ha funcionado bien


# Variables exógenas que miden el grado de preocupación
# ------------------------------------------------------------------------------
# PREOC
# Abril2020$PREOC2 <- Abril2020$PREOC
for(i in 1:length(Abril2020$PREOC)){
  if(Abril2020$PREOC[i]==1  | Abril2020$PREOC[i]==2) 
  {Abril2020$PREOC[i] <- 3} # Mucho y bastante
  
  else if(Abril2020$PREOC[i]==3) {Abril2020$PREOC[i] <- 2} # Regular
  
  else if(Abril2020$PREOC[i]==4 | Abril2020$PREOC[i]==5) 
  {Abril2020$PREOC[i] <- 1} # Poco o nada
  
  else if(Abril2020$PREOC[i]==8 | Abril2020$PREOC[i]==9) 
  {Abril2020$PREOC[i] <- -1} # NS/NC
}
# table(Abril2020$PREOC); table(Abril2020$PREOC2) # OK! Ha funcionado bien

# OPT
# Abril2020$OPT2 <- Abril2020$OPT
for(i in 1:length(Abril2020$OPT)){
  if(Abril2020$OPT[i]==1 | Abril2020$OPT[i]==2) {Abril2020$OPT[i] <- 0} 
  # Es optimista
  else if(Abril2020$OPT[i]==3 | Abril2020$OPT[i]==4 | Abril2020$OPT[i]==5) 
  {Abril2020$OPT[i] <- 1} # Es pesimista
  else if(Abril2020$OPT[i]==8 | Abril2020$OPT[i]==9) 
  {Abril2020$OPT[i] <- -1} # NS/NC
}
# table(Abril2020$OPT); table(Abril2020$OPT2) # OK! Ha funcionado bien

# COMPGEN
# Abril2020$COMPGEN2 <- Abril2020$COMPGEN
for(i in 1:length(Abril2020$COMPGEN)){
  if(Abril2020$COMPGEN[i]==1) {Abril2020$COMPGEN[i] <- 0} 
  # Cree la mayoría está reaccionando con civismo y solidaridad
  else if(Abril2020$COMPGEN[i]==2) {Abril2020$COMPGEN[i] <- 1} 
  # Mayoría poco cívica e indisciplinada
  else if(Abril2020$COMPGEN[i]==8 | Abril2020$COMPGEN[i]==9) 
  {Abril2020$COMPGEN[i] <- -1} # NS/NC
}
# table(Abril2020$COMPGEN); table(Abril2020$COMPGEN2) # OK! Ha funcionado bien

# Descargamos la base de datos arreglada
vars <- c("PROB_COVID", "PERS_COVID", 
          "CCAA", "TAMUNI", "SEXO", "EDAD", "EDAD.c", "ESTADO_CIVIL", "EXTR",
          "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", 
          "PREOC", "OPT", "COMPGEN")

dd_neta <- as.data.frame(Abril2020[,vars])

# Descargamos en CSV (o xlsx)
# write.csv2(dd_neta, "Abril2020_neta.csv")

