# ------------------------------------------------------------------------------
# Anexo 6: Estadística descriptiva univariante
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



# 6.1. Variables endógenas
# ------------------------------------------------------------------------------
setwd("C:/Users/DATA00/Desktop/TFG Laia/3. Estadística descriptiva/3.1. Univariante")

# OBLIG
# Var. endógena: OBLIG
freq(Sept2021$OBLIG, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("OBLIG_Sept2021.png")
pie (table(Sept2021$OBLIG), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("26.73", "73.27"), "%"))
legend("topright", legend = levels(Feb2022$OBLIG), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$OBLIG, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("OBLIG_Feb2022.png")
pie (table(Feb2022$OBLIG), col = c("#0378FF","#6BB745"), main = "Feb 2022", labels = paste0(c("47.67", "52.33"), "%"))
legend("topright", legend = levels(Feb2022$OBLIG), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# PROB_COVID
freq(Abril2020$PROB_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Abril 2020")
png("PROB_COVID_Abril2020.png")
pie (table(Abril2020$PROB_COVID), col = c("#0378FF","#6BB745"), main = "Abril 2020", labels = paste0(c("49.53", "50.47"), "%"))
legend("topright", legend = levels(Abril2020$PROB_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Enero2021$PROB_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("PROB_COVID_Enero2021.png")
pie (table(Enero2021$PROB_COVID), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("61.86", "38.14"), "%"))
legend("topright", legend = levels(Enero2021$PROB_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$PROB_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("PROB_COVID_Sept2021.png")
pie (table(Sept2021$PROB_COVID), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("86.72", "13.28"), "%"))
legend("topright", legend = levels(Feb2022$PROB_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$PROB_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("PROB_COVID_Feb2022.png")
pie (table(Feb2022$PROB_COVID), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("89.74", "10.26"), "%"))
legend("topright", legend = levels(Feb2022$PROB_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# PERS_COVID
freq(Abril2020$PERS_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Abril 2020")
png("PERS_COVID_Abril2020.png")
pie (table(Abril2020$PERS_COVID), col = c("#0378FF","#6BB745"), main = "Abril 2020", labels = paste0(c("57.8", "42.2"), "%"))
legend("topright", legend = levels(Abril2020$PERS_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Enero2021$PERS_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("PERS_COVID_Enero2021.png")
pie (table(Enero2021$PERS_COVID), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("61.42", "38.58"), "%"))
legend("topright", legend = levels(Enero2021$PERS_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$PERS_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("PERS_COVID_Sept2021.png")
pie (table(Sept2021$PERS_COVID), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("86.11", "13.89"), "%"))
legend("topright", legend = levels(Feb2022$PERS_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$PERS_COVID, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("PERS_COVID_Feb2022.png")
pie (table(Feb2022$PERS_COVID), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("87.02", "12.98"), "%"))
legend("topright", legend = levels(Feb2022$PERS_COVID), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()



# 6.2. Variables explicativas de tipo socioeconómico
# ------------------------------------------------------------------------------
# CC.AA. 
freq(Abril2020$CCAA, plot = F, col = rainbow(19), main = "BarPlot CCAA")
freq(Enero2021$CCAA, plot = F, col = rainbow(19), main = "BarPlot CCAA")
freq(Sept2021$CCAA, plot = F, col = rainbow(19), main = "BarPlot CCAA")
freq(Feb2022$CCAA, plot = F, col = rainbow(19), main = "BarPlot CCAA")

# TAMUNI
freq(Abril2020$TAMUNI, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot TAMUNI", axis.lty = 0)
png("TAMUNI_Abril20.png")
barplot(prop.table(table(Abril2020$TAMUNI)), col=palette("Tableau 10"),ylim=c(0,0.35))
legend("topright", legend = levels(Abril2020$TAMUNI), fill = palette("Tableau 10"), cex=0.4)
dev.off()

freq(Enero2021$TAMUNI, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot TAMUNI", axis.lty = 0)
png("TAMUNI_Enero21.png")
barplot(prop.table(table(Enero2021$TAMUNI)), col=palette("Tableau 10"),ylim=c(0,0.35))
legend("topright", legend = levels(Enero2021$TAMUNI), fill = palette("Tableau 10"), cex=0.7)
dev.off()

freq(Sept2021$TAMUNI, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot TAMUNI", axis.lty = 0)
png("TAMUNI_Sept21.png")
barplot(prop.table(table(Sept2021$TAMUNI)), col=palette("Tableau 10"),ylim=c(0,0.35))
legend("topright", legend = levels(Sept2021$TAMUNI), fill = palette("Tableau 10"), cex=0.4)
dev.off()

freq(Feb2022$TAMUNI, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot TAMUNI", axis.lty = 0)
png("TAMUNI_Feb22.png")
barplot(prop.table(table(Feb2022$TAMUNI)), col=palette("Tableau 10"),ylim=c(0,0.35))
legend("topright", legend = levels(Feb2022$TAMUNI), fill = palette("Tableau 10"), cex=0.4)
dev.off()

# SEXO
freq(Abril2020$SEXO, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Abril 2020")
png("SEXO_Abril2020.png")
pie (table(Abril2020$SEXO), col = c("#0378FF","#6BB745"), main = "Abril 2020", labels = paste0(c("48.47", "51.53"), "%"))
legend("topright", legend = levels(Abril2020$SEXO), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Enero2021$SEXO, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("SEXO_Enero2021.png")
pie (table(Enero2021$SEXO), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("48.68", "51.32"), "%"))
legend("topright", legend = levels(Enero2021$SEXO), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$SEXO, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("SEXO_Sept2021.png")
pie (table(Sept2021$SEXO), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("48.61", "51.39"), "%"))
legend("topright", legend = levels(Feb2022$SEXO), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$SEXO, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("SEXO_Feb2022.png")
pie (table(Feb2022$SEXO), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("49.12", "50.88"), "%"))
legend("topright", legend = levels(Feb2022$SEXO), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# EDAD
# Var. explicativa: EDAD
summary(Abril2020$EDAD); summary(Enero2021$EDAD)
summary(Sept2021$EDAD);  summary(Feb2022$EDAD)

sd(Abril2020$EDAD)     ; sd(Enero2021$EDAD)
sd(Sept2021$EDAD)      ; sd(Feb2022$EDAD)

edades <- data.frame(barometros = rep(c("Abril 20", "Enero 21", "Sept 21", "Feb 22"), c(3000, 3862, 3779, 3860)), 
                     edades= c(Abril2020$EDAD, Enero2021$EDAD, Sept2021$EDAD, Feb2022$EDAD))
png("EDAD.png")
boxplot(edades ~ barometros, edades, col= palette("Tableau 10"), main = "Boxplot EDAD")
dev.off()

png("histEDADAbril2020.png")
hist(Abril2020$EDAD, breaks= "Sturges", col=palette("Tableau 10")[1], main= "Abril 2020")
dev.off()

png("histEDADEnero2021.png")
hist(Enero2021$EDAD, breaks= "Sturges", col=palette("Tableau 10")[2], main= "Enero 2021")
dev.off()

png("histEDADSept2021.png")
hist(Sept2021$EDAD, breaks= "Sturges", col=palette("Tableau 10")[3], main= "Sept 2021")
dev.off()

png("histEDADFeb2022.png")
hist(Feb2022$EDAD, breaks= "Sturges", col=palette("Tableau 10")[4], main= "Feb 2022")
dev.off()

# ESTADO_CIVIL
freq(Abril2020$ESTADO_CIVIL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTADO_CIVIL", axis.lty = 0)
png("ESTADO_CIVIL_Abril20.png")
barplot(prop.table(table(Abril2020$ESTADO_CIVIL)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Abril2020$ESTADO_CIVIL), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$ESTADO_CIVIL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTADO_CIVIL", axis.lty = 0)
png("ESTADO_CIVIL_Enero21.png")
barplot(prop.table(table(Enero2021$ESTADO_CIVIL)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Enero2021$ESTADO_CIVIL), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$ESTADO_CIVIL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTADO_CIVIL", axis.lty = 0)
png("ESTADO_CIVIL_Sept21.png")
barplot(prop.table(table(Sept2021$ESTADO_CIVIL)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Sept2021$ESTADO_CIVIL), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$ESTADO_CIVIL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTADO_CIVIL", axis.lty = 0)
png("ESTADO_CIVIL_Feb22.png")
barplot(prop.table(table(Feb2022$ESTADO_CIVIL)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Feb2022$ESTADO_CIVIL), fill = palette("Tableau 10"), cex=1)
dev.off()

# EXTR
freq(Abril2020$EXTR, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Abril 2020")
png("EXTR_Abril2020.png")
pie (table(Abril2020$EXTR), col = c("#0378FF","#6BB745"), main = "Abril 2020", labels = paste0(c("98.53", "1.46"), "%"))
legend("topright", legend = levels(Abril2020$EXTR), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Enero2021$EXTR, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("EXTR_Enero2021.png")
pie (table(Enero2021$EXTR), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("97.62", "2.38"), "%"))
legend("topright", legend = levels(Enero2021$EXTR), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$EXTR, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("EXTR_Sept2021.png")
pie (table(Sept2021$EXTR), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("96.51", "3.49"), "%"))
legend("topright", legend = levels(Feb2022$EXTR), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$EXTR, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("EXTR_Feb2022.png")
pie (table(Feb2022$EXTR), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("96.76", "3.24"), "%"))
legend("topright", legend = levels(Feb2022$EXTR), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# ORIENT
freq(Abril2020$ORIENT, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ORIENT", axis.lty = 0)
png("ORIENT_Abril20.png")
barplot(prop.table(table(Abril2020$ORIENT)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Abril2020$ORIENT), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$ORIENT, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ORIENT", axis.lty = 0)
png("ORIENT_Enero21.png")
barplot(prop.table(table(Enero2021$ORIENT)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Enero2021$ORIENT), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$ORIENT, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ORIENT", axis.lty = 0)
png("ORIENT_Sept21.png")
barplot(prop.table(table(Sept2021$ORIENT)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Sept2021$ORIENT), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$ORIENT, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ORIENT", axis.lty = 0)
png("ORIENT_Feb22.png")
barplot(prop.table(table(Feb2022$ORIENT)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Feb2022$ORIENT), fill = palette("Tableau 10"), cex=1)
dev.off()

# ESTUD
freq(Abril2020$ESTUD, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTUD", axis.lty = 0)
png("ESTUD_Abril20.png")
barplot(prop.table(table(Abril2020$ESTUD)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Abril2020$ESTUD), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$ESTUD, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTUD", axis.lty = 0)
png("ESTUD_Enero21.png")
barplot(prop.table(table(Enero2021$ESTUD)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Enero2021$ESTUD), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$ESTUD, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTUD", axis.lty = 0)
png("ESTUD_Sept21.png")
barplot(prop.table(table(Sept2021$ESTUD)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Sept2021$ESTUD), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$ESTUD, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot ESTUD", axis.lty = 0)
png("ESTUD_Feb22.png")
barplot(prop.table(table(Feb2022$ESTUD)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Feb2022$ESTUD), fill = palette("Tableau 10"), cex=1)
dev.off()

# RELIG
freq(Abril2020$RELIG, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot RELIG", axis.lty = 0)
png("RELIG_Abril20.png")
barplot(prop.table(table(Abril2020$RELIG)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Abril2020$RELIG), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$RELIG, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot RELIG", axis.lty = 0)
png("RELIG_Enero21.png")
barplot(prop.table(table(Enero2021$RELIG)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Enero2021$RELIG), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$RELIG, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot RELIG", axis.lty = 0)
png("RELIG_Sept21.png")
barplot(prop.table(table(Sept2021$RELIG)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Sept2021$RELIG), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$RELIG, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot RELIG", axis.lty = 0)
png("RELIG_Feb22.png")
barplot(prop.table(table(Feb2022$RELIG)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Feb2022$RELIG), fill = palette("Tableau 10"), cex=1)
dev.off()

# SIT_LAB
freq(Abril2020$SIT_LAB, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_LAB", axis.lty = 0)
png("SIT_LAB_Abril20.png")
barplot(prop.table(table(Abril2020$SIT_LAB)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Abril2020$SIT_LAB), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$SIT_LAB, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_LAB", axis.lty = 0)
png("SIT_LAB_Enero21.png")
barplot(prop.table(table(Enero2021$SIT_LAB)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Enero2021$SIT_LAB), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$SIT_LAB, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_LAB", axis.lty = 0)
png("SIT_LAB_Sept21.png")
barplot(prop.table(table(Sept2021$SIT_LAB)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Sept2021$SIT_LAB), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$SIT_LAB, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_LAB", axis.lty = 0)
png("SIT_LAB_Feb22.png")
barplot(prop.table(table(Feb2022$SIT_LAB)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Feb2022$SIT_LAB), fill = palette("Tableau 10"), cex=1)
dev.off()

# SIT_ECON
freq(Abril2020$SIT_ECON, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_ECON", axis.lty = 0)
png("SIT_ECON_Abril20.png")
barplot(prop.table(table(Abril2020$SIT_ECON)), col=palette("Tableau 10"),ylim=c(0,0.8))
legend("topright", legend = levels(Abril2020$SIT_ECON), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$SIT_ECON, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_ECON", axis.lty = 0)
png("SIT_ECON_Enero21.png")
barplot(prop.table(table(Enero2021$SIT_ECON)), col=palette("Tableau 10"),ylim=c(0,0.8))
legend("topright", legend = c("Mala o muy mala", "Regular", "Buena o muy buena"), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$SIT_ECON, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_ECON", axis.lty = 0)
png("SIT_ECON_Sept21.png")
barplot(prop.table(table(Sept2021$SIT_ECON)), col=palette("Tableau 10"),ylim=c(0,0.8))
legend("topright", legend = levels(Sept2021$SIT_ECON), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$SIT_ECON, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot SIT_ECON", axis.lty = 0)
png("SIT_ECON_Feb22.png")
barplot(prop.table(table(Feb2022$SIT_ECON)), col=palette("Tableau 10"),ylim=c(0,0.8))
legend("topright", legend = levels(Feb2022$SIT_ECON), fill = palette("Tableau 10"), cex=1)
dev.off()

# CLASE
freq(Abril2020$CLASE, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot CLASE", axis.lty = 0)
png("CLASE_Abril20.png")
barplot(prop.table(table(Abril2020$CLASE)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Abril2020$CLASE), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$CLASE, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot CLASE", axis.lty = 0)
png("CLASE_Enero21.png")
barplot(prop.table(table(Enero2021$CLASE)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Enero2021$CLASE), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$CLASE, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot CLASE", axis.lty = 0)
png("CLASE_Sept21.png")
barplot(prop.table(table(Sept2021$CLASE)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Sept2021$CLASE), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$CLASE, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot CLASE", axis.lty = 0)
png("CLASE_Feb22.png")
barplot(prop.table(table(Feb2022$CLASE)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Feb2022$CLASE), fill = palette("Tableau 10"), cex=1)
dev.off()

# PREOC
freq(Abril2020$PREOC, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot PREOC", axis.lty = 0)
png("PREOC_Abril20.png")
barplot(prop.table(table(Abril2020$PREOC)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Abril2020$PREOC), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Enero2021$PREOC, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot PREOC", axis.lty = 0)
png("PREOC_Enero21.png")
barplot(prop.table(table(Enero2021$PREOC)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Enero2021$PREOC), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$PREOC, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot PREOC", axis.lty = 0)
png("PREOC_Sept21.png")
barplot(prop.table(table(Sept2021$PREOC)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Sept2021$PREOC), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Feb2022$PREOC, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot PREOC", axis.lty = 0)
png("PREOC_Feb22.png")
barplot(prop.table(table(Feb2022$PREOC)), col=palette("Tableau 10"),ylim=c(0,0.6))
legend("topright", legend = levels(Feb2022$PREOC), fill = palette("Tableau 10"), cex=1)
dev.off()

# OPT
freq(Abril2020$OPT, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Abril 2020")
png("OPT_Abril2020.png")
pie (table(Abril2020$OPT), col = c("#0378FF","#6BB745"), main = "Abril 2020", labels = paste0(c("76.73", "23.27"), "%"))
legend("topright", legend = c("Optimista", "Pesimista"), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Enero2021$OPT, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("OPT_Enero2021.png")
pie (table(Enero2021$OPT), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("14.68", "85.32"), "%"))
legend("topright", legend = c("Optimista", "Pesimista"), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$OPT, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("OPT_Sept2021.png")
pie (table(Sept2021$OPT), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("73.33", "26.67"), "%"))
legend("topright", legend = c("Optimista", "Pesimista"), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$OPT, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("OPT_Feb2022.png")
pie (table(Feb2022$OPT), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("78.68", "21.32"), "%"))
legend("topright", legend = levels(Feb2022$OPT), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# AFEC_P
freq(Enero2021$AFEC_P, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("AFEC_P_Enero2021.png")
pie (table(Enero2021$AFEC_P), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("37.31", "62.69"), "%"))
legend("topright", legend = levels(Enero2021$AFEC_P), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$AFEC_P, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("AFEC_P_Sept2021.png")
pie (table(Sept2021$AFEC_P), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("47.05", "52.95"), "%"))
legend("topright", legend = levels(Feb2022$AFEC_P), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$AFEC_P, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("AFEC_P_Feb2022.png")
pie (table(Feb2022$AFEC_P), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("53.19", "46.81"), "%"))
legend("topright", legend = levels(Feb2022$AFEC_P), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# AF_ANIMICO
freq(Enero2021$AF_ANIMICO, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("AF_ANIMICO_Enero2021.png")
pie (table(Enero2021$AF_ANIMICO), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("48.99", "51.01"), "%"))
legend("topright", legend = levels(Enero2021$AF_ANIMICO), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$AF_ANIMICO, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("AF_ANIMICO_Sept2021.png")
pie (table(Sept2021$AF_ANIMICO), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("61.47", "38.53"), "%"))
legend("topright", legend = levels(Feb2022$AF_ANIMICO), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$AF_ANIMICO, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("AF_ANIMICO_Feb2022.png")
pie (table(Feb2022$AF_ANIMICO), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("63.29", "36.71"), "%"))
legend("topright", legend = levels(Feb2022$AF_ANIMICO), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# AF_SALUD
freq(Enero2021$AF_SALUD, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("AF_SALUD_Enero2021.png")
pie (table(Enero2021$AF_SALUD), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("86.72", "13.28"), "%"))
legend("topright", legend = levels(Enero2021$AF_SALUD), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$AF_SALUD, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("AF_SALUD_Sept2021.png")
pie (table(Sept2021$AF_SALUD), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("87.75", "12.25"), "%"))
legend("topright", legend = levels(Feb2022$AF_SALUD), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$AF_SALUD, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("AF_SALUD_Feb2022.png")
pie (table(Feb2022$AF_SALUD), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("87.49", "12.51"), "%"))
legend("topright", legend = levels(Feb2022$AF_SALUD), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# AF_ECON
freq(Enero2021$AF_ECON, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("AF_ECON_Enero2021.png")
pie (table(Enero2021$AF_ECON), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("76.13", "23.87"), "%"))
legend("topright", legend = levels(Enero2021$AF_ECON), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$AF_ECON, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("AF_ECON_Sept2021.png")
pie (table(Sept2021$AF_ECON), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("80.37", "19.63"), "%"))
legend("topright", legend = levels(Feb2022$AF_ECON), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$AF_ECON, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("AF_ECON_Feb2022.png")
pie (table(Feb2022$AF_ECON), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("85.26", "14.74"), "%"))
legend("topright", legend = levels(Feb2022$AF_ECON), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# AF_LIBERTAD
freq(Enero2021$AF_LIBERTAD, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("AF_LIBERTAD_Enero2021.png")
pie (table(Enero2021$AF_LIBERTAD), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("62.58", "37.42"), "%"))
legend("topright", legend = levels(Enero2021$AF_LIBERTAD), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$AF_LIBERTAD, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("AF_LIBERTAD_Sept2021.png")
pie (table(Sept2021$AF_LIBERTAD), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("66.34", "33.66"), "%"))
legend("topright", legend = levels(Feb2022$AF_LIBERTAD), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$AF_LIBERTAD, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("AF_LIBERTAD_Feb2022.png")
pie (table(Feb2022$AF_LIBERTAD), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("69.51", "30.49"), "%"))
legend("topright", legend = levels(Feb2022$AF_LIBERTAD), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# AF_OTROS
freq(Enero2021$AF_OTROS, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("AF_OTROS_Enero2021.png")
pie (table(Enero2021$AF_OTROS), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("86.2", "13.8"), "%"))
legend("topright", legend = levels(Enero2021$AF_OTROS), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$AF_OTROS, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("AF_OTROS_Sept2021.png")
pie (table(Sept2021$AF_OTROS), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("76.45", "23.55"), "%"))
legend("topright", legend = levels(Feb2022$AF_OTROS), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$AF_OTROS, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("AF_OTROS_Feb2022.png")
pie (table(Feb2022$AF_OTROS), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("77.07", "22.93"), "%"))
legend("topright", legend = levels(Feb2022$AF_OTROS), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# COMPGEN
freq(Abril2020$COMPGEN, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Abril 2020")
png("COMPGEN_Abril2020.png")
pie (table(Abril2020$COMPGEN), col = c("#0378FF","#6BB745"), main = "Abril 2020", labels = paste0(c("94.4", "5.6"), "%"))
legend("topright", legend = levels(Abril2020$COMPGEN), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Enero2021$COMPGEN, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("COMPGEN_Enero2021.png")
pie (table(Enero2021$COMPGEN), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("51.92", "48.08"), "%"))
legend("topright", legend = c("Mayoría cívica y solidaria", "Mayoría incivica y poco solidaria"), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$COMPGEN, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("COMPGEN_Sept2021.png")
pie (table(Sept2021$COMPGEN), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("63.61", "36.39"), "%"))
legend("topright", legend = levels(Feb2022$COMPGEN), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$COMPGEN, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("COMPGEN_Feb2022.png")
pie (table(Feb2022$COMPGEN), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("77.49", "22.51"), "%"))
legend("topright", legend = levels(Feb2022$COMPGEN), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()





# 6.3. Variables explicativas relacionadas con la enfermedad
# ------------------------------------------------------------------------------
# SINTOM
freq(Enero2021$SINTOM, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("SINTOM_Enero2021.png")
pie (table(Enero2021$SINTOM), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("77.94", "22.06"), "%"))
legend("topright", legend = levels(Enero2021$SINTOM), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$SINTOM, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("SINTOM_Sept2021.png")
pie (table(Sept2021$SINTOM), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("66.13", "33.87"), "%"))
legend("topright", legend = levels(Feb2022$SINTOM), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$SINTOM, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("SINTOM_Feb2022.png")
pie (table(Feb2022$SINTOM), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("56.37", "43.63"), "%"))
legend("topright", legend = levels(Feb2022$SINTOM), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# DIAG
freq(Enero2021$DIAG, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Enero 2021")
png("DIAG_Enero2021.png")
pie (table(Enero2021$DIAG), col = c("#0378FF","#6BB745"), main = "Enero 2021", labels = paste0(c("94.85", "5.15"), "%"))
legend("topright", legend = levels(Enero2021$DIAG), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Sept2021$DIAG, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Sept 2021")
png("DIAG_Sept2021.png")
pie (table(Sept2021$DIAG), col = c("#0378FF","#6BB745"), main = "Sept 2021", labels = paste0(c("89.63", "10.37"), "%"))
legend("topright", legend = levels(Feb2022$DIAG), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

freq(Feb2022$DIAG, plot = FALSE, col = c("#0378FF","#6BB745"), main = "Feb 2022")
png("DIAG_Feb2022.png")
pie (table(Feb2022$DIAG), col = c("#0378FF","#6BB745"), main = "Feb2022", labels = paste0(c("82.31", "17.69"), "%"))
legend("topright", legend = levels(Feb2022$DIAG), fill = c("#0378FF","#6BB745"), cex=0.7)
dev.off()

# EVOL
freq(Enero2021$EVOL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot EVOL", axis.lty = 0)
png("EVOL_Enero21.png")
barplot(prop.table(table(Enero2021$EVOL)), col=palette("Tableau 10"),ylim=c(0,1))
legend("topleft", legend = levels(Enero2021$EVOL), fill = palette("Tableau 10"), cex=1)
dev.off()

freq(Sept2021$EVOL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot EVOL", axis.lty = 0)
png("EVOL_Sept21.png")
barplot(prop.table(table(Sept2021$EVOL)), col=palette("Tableau 10"),ylim=c(0,1))
legend("topright", legend = c("No covid o asintomático", "Diagnóstico, leve", "Diagnóstico, importante sin hospitalización
", "Diagnóstico importante, Con hospitalización"), fill = palette("Tableau 10"), cex=0.7)
dev.off()

freq(Feb2022$EVOL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "BarPlot EVOL", axis.lty = 0)
png("EVOL_Feb22.png")
barplot(prop.table(table(Feb2022$EVOL)), col=palette("Tableau 10"),ylim=c(0,1))
legend("topright", legend = levels(Feb2022$EVOL), fill = palette("Tableau 10"), cex=1)
dev.off()

