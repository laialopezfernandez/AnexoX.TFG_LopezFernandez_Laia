# ------------------------------------------------------------------------------
# Anexo 7: Estadística descriptiva bivariante
# ------------------------------------------------------------------------------

# Carga de las librerías
library(easypackages)
paq <-  c("descr", "RColorBrewer", "grDevices"); libraries (paq)

# Importación de las bases de datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/5. Barometros limpios")
Abril2020 <- readRDS("Abril2020_imp.rds", refhook = NULL)
Enero2021 <- readRDS("Enero2021_imp.rds", refhook = NULL) 
Sept2021  <- readRDS("Sept2021_imp.rds", refhook = NULL)  
Feb2022   <- readRDS("Feb2022_imp.rds", refhook = NULL)  



# Objetivo 1: OBLIG
# ------------------------------------------------------------------------------
# Debemos aplicar el código siguiente a los barómetros:
# Sept2021 y Feb2022

vars <- names(Sept2021)
barometro <- Sept2021

# Graficos
setwd("C:/Users/DATA00/Desktop/TFG Laia/3. Estadística descriptiva/3.2. Bivariante/OBLIG/Sept2021")

for (va in vars[-c(1,2,3,4)]){ #-1:3 en Abril
  png(paste0(va, "_OBLIG_Sept2021.png"))
  if (!is.factor(barometro[,va])){
    boxplot(as.formula(paste0(va,"~OBLIG")), barometro , 
            main="Septiembre 2021", col = c("#0378FF","#6BB745"), 
            horizontal=T, outpch= 16, outcol = "#FF6A6A")
  }else{
    barplot(prop.table(table(barometro$OBLIG, barometro[,va]),2), 
            main="Septiembre 2021", col = c("#0378FF","#6BB745"), 
            xlab="OBLIG")
  }
  dev.off()
}

# Se repiten algunos gráficos por cuestiones de escala
png("CCAA_OBLIG_Sept2021.png")
x <- barplot(prop.table(table(Sept2021$OBLIG, Sept2021[,"CCAA"]),2), 
        main="Septiembre 2021",col = c("#0378FF","#6BB745"), 
        xlab="OBLIG", xaxt="n")
text(cex=0.7, x=x, y=-0.01, levels(Sept2021[,"CCAA"]), xpd=TRUE, srt=45, pos=2)
dev.off()

png("TAMUNI_OBLIG_Sept2021.png")
x <- barplot(prop.table(table(Sept2021$OBLIG, Sept2021[,"TAMUNI"]),2), 
             main="Septiembre 2021", col = c("#0378FF","#6BB745"), 
             xlab="OBLIG", xaxt="n")
text(cex=0.8, x=x, y=-0.03, levels(Sept2021[,"TAMUNI"]), xpd=TRUE, srt=45, pos=2)
dev.off()

png("EDAD.c_OBLIG_Sept2021.png")
x <- barplot(prop.table(table(Sept2021$OBLIG, Sept2021[,"EDAD.c"]),2), 
             main="Septiembre 2021", col = c("#0378FF","#6BB745"), 
             xlab="OBLIG", xaxt="n")
text(cex=0.8, x=x, y=-0.03, levels(Sept2021[,"EDAD.c"]), xpd=TRUE, srt=45, pos=2)
dev.off()

# Tablas y test de independencia
# Se aplica este codigo para todas las combinaciones entre variables y 
# barómetros distintas

barometro <- Sept2021
va <- "CCAA" 

(tabla_abs <- table(barometro[,va], barometro$OBLIG,  useNA = "no"))
colSums(tabla_abs)
rowSums(tabla_abs)

(tabla_rel <- round(tabla_abs/rowSums(tabla_abs)*100, 2))
round(colSums(tabla_abs)/sum(tabla_abs)*100,2)

chisq.test(tabla_abs)




# Objetivo 2.1: PROB_COVID
# ------------------------------------------------------------------------------
# Debemos aplicar el código siguiente a todos los barómetros:

vars <- names(Feb2022)
barometro <- Feb2022

# Graficos
setwd("C:/Users/DATA00/Desktop/TFG Laia/3. Estadística descriptiva/3.2. Bivariante/PROB_COVID/Feb2022")

for (va in vars[-c(1,2,3,4)]){ #-1:3 en Abril y Enero
  png(paste0(va, "_PROB_COVID_Feb2022.png"))
  if (!is.factor(barometro[,va])){
    boxplot(as.formula(paste0(va,"~PROB_COVID")), barometro , 
            main="Feb 2022", col = c("#0378FF","#6BB745"), 
            horizontal=T, outpch= 16, outcol = "#FF6A6A")
  }else{
    barplot(prop.table(table(barometro$PROB_COVID, barometro[,va]),2), 
            main="Feb 2022", col = c("#0378FF","#6BB745"), 
            xlab="PROB_COVID")
  }
  dev.off()
}

# Se repiten algunos gráficos por cuestiones de escala
png("CCAA_PROB_COVID_Feb2022.png")
x <- barplot(prop.table(table(Feb2022$PROB_COVID, Feb2022[,"CCAA"]),2), 
             main="Feb 2022",col = c("#0378FF","#6BB745"), 
             xlab="", xaxt="n")
text(cex=0.7, x=x, y=-0.01, levels(Feb2022[,"CCAA"]), xpd=TRUE, srt=45, pos=2)
dev.off()

png("TAMUNI_PROB_COVID_Feb2022.png")
x <- barplot(prop.table(table(Feb2022$PROB_COVID, Feb2022[,"TAMUNI"]),2), 
             main="Feb 2022", col = c("#0378FF","#6BB745"), 
             xlab="", xaxt="n")
text(cex=0.8, x=x, y=-0.03, levels(Feb2022[,"TAMUNI"]), xpd=TRUE, srt=45, pos=2)
dev.off()

png("EDAD.c_PROB_COVID_Feb2022.png")
x <- barplot(prop.table(table(Feb2022$PROB_COVID, Feb2022[,"EDAD.c"]),2), 
             main="Feb 2022", col = c("#0378FF","#6BB745"), 
             xlab="", xaxt="n")
text(cex=0.8, x=x, y=-0.03, levels(Feb2022[,"EDAD.c"]), xpd=TRUE, srt=45, pos=2)
dev.off()

# Tablas y test de independencia
# Se aplica este codigo para todas las combinaciones entre variables y 
# barómetros distintas
barometro <- Feb2022
va <- ""

(tabla_abs <- table(barometro[,va], barometro$PROB_COVID,  useNA = "no"))
sum(colSums(tabla_abs))
rowSums(tabla_abs)

(tabla_rel <- round(tabla_abs/rowSums(tabla_abs)*100, 2))
round(colSums(tabla_abs)/sum(tabla_abs)*100,2)


chisq.test(tabla_abs)



# Objetivo 2.2: PERS_COVID
# ------------------------------------------------------------------------------
# Debemos aplicar el código siguiente a todos los barómetros:

vars <- names(Abril2020)
barometro <- Abril2020

# Graficos
setwd("C:/Users/DATA00/Desktop/TFG Laia/3. Estadística descriptiva/3.2. Bivariante/PERS_COVID/Abril2020")

for (va in vars[-c(1,2,3, 4)]){ #-1:3 en Abril y Enero
  png(paste0(va, "_PERS_COVID_Abril2020.png"))
  if (!is.factor(barometro[,va])){
    boxplot(as.formula(paste0(va,"~PERS_COVID")), barometro , 
            main="Abril 2020", col = c("#0378FF","#6BB745"), 
            horizontal=T, outpch= 16, outcol = "#FF6A6A")
  }else{
    barplot(prop.table(table(barometro$PERS_COVID, barometro[,va]),2), 
            main="Abril 2020", col = c("#0378FF","#6BB745"), 
            xlab="PERS_COVID")
  }
  dev.off()
}

# Se repiten algunos gráficos por cuestiones de escala
png("CCAA_PERS_COVID_Abril2020.png")
x <- barplot(prop.table(table(Abril2020$PERS_COVID, Abril2020[,"CCAA"]),2), 
             main="Abril 2020", col = c("#0378FF","#6BB745"), 
             xlab="", xaxt="n")
text(cex=0.7, x=x, y=-0.01, levels(Abril2020[,"CCAA"]), xpd=TRUE, srt=45, pos=2)
dev.off()

png("TAMUNI_PERS_COVID_Abril2020.png")
x <- barplot(prop.table(table(Abril2020$PERS_COVID, Abril2020[,"TAMUNI"]),2), 
             main="Abril 2020", col = c("#0378FF","#6BB745"), 
             xlab="", xaxt="n")
text(cex=0.8, x=x, y=-0.03, levels(Abril2020[,"TAMUNI"]), xpd=TRUE, srt=45, pos=2)
dev.off()

png("EDAD.c_PERS_COVID_Abril2020.png")
x <- barplot(prop.table(table(Abril2020$PERS_COVID, Abril2020[,"EDAD.c"]),2), 
             main="Abril 2020", col = c("#0378FF","#6BB745"), 
             xlab="", xaxt="n")
text(cex=0.8, x=x, y=-0.03, levels(Abril2020[,"EDAD.c"]), xpd=TRUE, srt=45, pos=2)
dev.off()



# Tablas y test de independencia
# Se aplica este codigo para todas las combinaciones entre variables y 
# barómetros distintas
barometro <- Abril2020
va <- "CCAA"

(tabla_abs <- table(barometro[,va], barometro$PERS_COVID,  useNA = "no"))
colSums(tabla_abs)
rowSums(tabla_abs)

(tabla_rel <- round(tabla_abs/rowSums(tabla_abs)*100, 2))
round(colSums(tabla_abs)/sum(tabla_abs)*100,2)


chisq.test(tabla_abs)

