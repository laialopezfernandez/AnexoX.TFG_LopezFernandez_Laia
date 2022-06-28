# ------------------------------------------------------------------------------
# Anexo 5: Validación de las imputaciones
# ------------------------------------------------------------------------------

# Carga de las librerías
library(easypackages)
paq <-  c("mice"); libraries (paq)

# Carga de la base de datos
original <- na.omit(Feb2022) # Base de dades completa (Ejecutar Anexo 4)
nas      <- original

set.seed(20077735)
nas[sample(1:nrow(nas), trunc(0.072*2576)),  "OBLIG" ]        <- NA
nas[sample(1:nrow(nas), trunc(0.0021*2576)), "ESTADO_CIVIL" ] <- NA
nas[sample(1:nrow(nas), trunc(0.0868*2576)), "ORIENT" ]       <- NA
nas[sample(1:nrow(nas), trunc(0.0073*2576)), "ESTUD" ]        <- NA
nas[sample(1:nrow(nas), trunc(0.0205*2576)), "RELIG" ]        <- NA
nas[sample(1:nrow(nas), trunc(0.0013*2576)), "SIT_LAB" ]      <- NA
nas[sample(1:nrow(nas), trunc(0.0093*2576)), "SIT_ECON" ]     <- NA
nas[sample(1:nrow(nas), trunc(0.0756*2576)), "CLASE" ]        <- NA
nas[sample(1:nrow(nas), trunc(0.0057*2576)), "PREOC" ]        <- NA
nas[sample(1:nrow(nas), trunc(0.1101*2576)), "OPT" ]          <- NA
nas[sample(1:nrow(nas), trunc(0.0047*2576)), "AFEC_P" ]       <- NA
nas[sample(1:nrow(nas), trunc(0.0744*2576)), "COMPGEN" ]      <- NA
nas[sample(1:nrow(nas), trunc(0.0013*2576)), "SINTOM" ]       <- NA

sum(colSums(is.na(nas)))

# Imputació
init = mice(nas, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("EDAD")]="norm" 
meth[c("OBLIG", "PROB_COVID", "PERS_COVID", "SEXO", "EXTR", "OPT", "AFEC_P", "AF_ANIMICO", "AF_SALUD", "AF_ECON", "AF_LIBERTAD", "AF_OTROS", "COMPGEN", "SINTOM", "DIAG")]="logreg" 
meth[c("CCAA", "TAMUNI", "EDAD.c", "ESTADO_CIVIL", "ORIENT", "ESTUD", "RELIG", "SIT_LAB", "SIT_ECON", "CLASE", "PREOC", "EVOL")]="polyreg"

set.seed(2022)
imputed = mice(nas, method=meth, predictorMatrix=predM, m=5)
imputada <- complete (imputed)

# Distribuciones
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/02. Tratamiento missings")

   # OBLIG
   freq(original$OBLIG, plot = FALSE, col = c("#0378FF","#6BB745"), main = "OBLIG original")
   png("OBLIG_original.png")
     pie (table(original$OBLIG), col = c("#0378FF","#6BB745"), main = "OBLIG (original)", labels = paste0(c("47.55", "52.45"), "%"))
     legend("topright", legend = levels(original$OBLIG), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   freq(imputada$OBLIG, plot = FALSE, col = c("#0378FF","#6BB745"), main = "OBLIG imputada")
   png("OBLIG_imputada.png")
   pie (table(imputada$OBLIG), col = c("#0378FF","#6BB745"), main = "OBLIG (imputada)", labels = paste0(c("47.75", "52.25"), "%"))
   legend("topright", legend = levels(imputada$OBLIG), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()

  
   # ESTADO_CIVIL
   freq(original$ESTADO_CIVIL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "ESTADO_CIVIL original", axis.lty = 0)
   png("ESTADO_CIVIL_original.png")
   barplot(prop.table(table(original$ESTADO_CIVIL)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "ESTADO_CIVIL original")
   legend("topright", legend = levels(original$ESTADO_CIVIL), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.5958, "54.58%")
   text(1.9, 0.373, "32.3%")
   text(3.15, 0.1012, "5.12%")
   text(4.35, 0.13, "8.00%")
   dev.off()
   
   freq(imputada$ESTADO_CIVIL, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "ESTADO_CIVIL imputada", axis.lty = 0)
   png("ESTADO_CIVIL_imputada.png")
   barplot(prop.table(table(imputada$ESTADO_CIVIL)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "ESTADO_CIVIL imputada")
   legend("topright", legend = levels(imputada$ESTADO_CIVIL), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.5958, "54.65%")
   text(1.9, 0.373, "32.22%")
   text(3.15, 0.1012, "5.12%")
   text(4.35, 0.13, "8.00%")
   dev.off()
   
   # ORIENT
   freq(original$ORIENT, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "ORIENT original", axis.lty = 0)
   png("ORIENT_original.png")
   barplot(prop.table(table(original$ORIENT)), col=palette("Tableau 10"),ylim=c(0,0.6), main = "ORIENT original")
   legend("topright", legend = levels(original$ORIENT), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.2209, "18.09%")
   text(1.9, 0.3148, "26.48%")
   text(3.15, 0.4246, "37.46%")
   text(4.35, 0.1828, "13.28%")
   text(5.55, 0.097, "4.70%")
   dev.off()
   
   freq(imputada$ORIENT, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "ORIENT imputada", axis.lty = 0)
   png("ORIENT_imputada.png")
   barplot(prop.table(table(imputada$ORIENT)), col=palette("Tableau 10"),ylim=c(0,0.6), main = "ORIENT imputada")
   legend("topright", legend = levels(imputada$ORIENT), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.2209, "18.48%")
   text(1.9, 0.3148, "26.24%")
   text(3.15, 0.4246, "36.88%")
   text(4.35, 0.1828, "13.59%")
   text(5.55, 0.097, "4.81%")
   dev.off()
   
   # ESTUD
   freq(original$ESTUD, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "ESTUD original", axis.lty = 0)
   png("ESTUD_original.png")
   barplot(prop.table(table(original$ESTUD)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "ESTUD original")
   legend("topright", legend = levels(original$ESTUD), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.1106, "6.06%")
   text(1.9, 0.5453, "49.53%")
   text(3.15, 0.4941, "44.41%")
   dev.off()
   
   freq(imputada$ESTUD, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "ESTUD imputada", axis.lty = 0)
   png("ESTUD_imputada.png")
   barplot(prop.table(table(imputada$ESTUD)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "ESTUD imputada")
   legend("topright", legend = levels(imputada$ESTUD), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.1106, "6.13%")
   text(1.9, 0.5453, "49.38%")
   text(3.15, 0.4941, "44.49%")
   dev.off()
   
   # RELIG
   freq(original$RELIG, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "RELIG original", axis.lty = 0)
   png("RELIG_original.png")
   barplot(prop.table(table(original$RELIG)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "RELIG original")
   legend("topright", legend = levels(original$RELIG), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.6106, "56.06%")
   text(1.9, 0.0694, "1.94%")
   text(3.15, 0.47, "42.00%")
   dev.off()
   
   freq(imputada$RELIG, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "RELIG imputada", axis.lty = 0)
   png("RELIG_imputada.png")
   barplot(prop.table(table(imputada$RELIG)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "RELIG imputada")
   legend("topright", legend = levels(imputada$RELIG), fill = palette("Tableau 10"), cex=1)
   text(0.75, 0.6106, "56.09%")
   text(1.9, 0.0694, "1.98%")
   text(3.15, 0.47, "41.93%")
   dev.off()
   
   # SIT_LAB
   freq(original$SIT_LAB, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "SIT_LAB original", axis.lty = 0)
   png("SIT_LAB_original.png")
   barplot(prop.table(table(original$SIT_LAB)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "SIT_LAB original")
   text(0.75, 0.6292, "57.92%")
   text(1.9, 0.2907, "24.07%")
   text(3.15, 0.1498, "9.98%")
   text(4.35, 0.0919, "4.19%")
   text(5.55, 0.0884, "3.84%")
   legend("topright", legend = levels(original$SIT_LAB), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   freq(imputada$SIT_LAB, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "SIT_LAB imputada", axis.lty = 0)
   png("SIT_LAB_imputada.png")
   barplot(prop.table(table(imputada$SIT_LAB)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "SIT_LAB imputada")
   text(0.75, 0.6292, "57.92%")
   text(1.9, 0.2907, "24.07%")
   text(3.15, 0.1498, "9.98%")
   text(4.35, 0.0919, "4.19%")
   text(5.55, 0.0884, "3.84%")
   legend("topright", legend = levels(imputada$SIT_LAB), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   # SIT_ECON
   freq(original$SIT_ECON, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "SIT_ECON original", axis.lty = 0)
   png("SIT_ECON_original.png")
   barplot(prop.table(table(original$SIT_ECON)), col=palette("Tableau 10"),ylim=c(0,1), main = "SIT_ECON original")
   text(0.75, 0.2616, "21.16%")
   text(1.9, 0.1385, "8.85%")
   text(3.15, 0.7499, "69.99%")
   legend("topright", legend = levels(original$SIT_ECON), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   freq(imputada$SIT_ECON, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "SIT_ECON imputada", axis.lty = 0)
   png("SIT_ECON_imputada.png")
   barplot(prop.table(table(imputada$SIT_ECON)), col=palette("Tableau 10"),ylim=c(0,1), main = "SIT_ECON imputada")
   text(0.75, 0.2616, "21.2%")
   text(1.9, 0.1385, "8.85%")
   text(3.15, 0.7499, "69.95%")
   legend("topright", legend = levels(imputada$SIT_ECON), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   # CLASE
   freq(original$CLASE, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "CLASE original", axis.lty = 0)
   png("CLASE_original.png")
   barplot(prop.table(table(original$CLASE)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "CLASE original")
   text(0.75, 0.3454, "29.54%")
   text(1.9, 0.6785, "62.85%")
   text(3.15, 0.1261, "7.61%")
   legend("topright", legend = levels(original$CLASE), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   freq(imputada$CLASE, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "CLASE imputada", axis.lty = 0)
   png("CLASE_imputada.png")
   barplot(prop.table(table(imputada$CLASE)), col=palette("Tableau 10"),ylim=c(0,0.7), main = "CLASE imputada")
   text(0.75, 0.3454, "29.12%")
   text(1.9, 0.6785, "63.39%")
   text(3.15, 0.1261, "7.49%")
   legend("topright", legend = levels(imputada$CLASE), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   # PREOC
   freq(original$PREOC, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "PREOC original", axis.lty = 0)
   png("PREOC_original.png")
   barplot(prop.table(table(original$PREOC)), col=palette("Tableau 10"),ylim=c(0,1), main = "PREOC original")
   text(0.75, 0.2305, "18.05%")
   text(1.9, 0.0675, "1.75%")
   text(3.15, 0.8520, "80.20%")
   legend("top", legend = levels(original$PREOC), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   freq(imputada$PREOC, plot = FALSE, y.axis= "percent", col=palette("Tableau 10"), main = "PREOC imputada", axis.lty = 0)
   png("PREOC_imputada.png")
   barplot(prop.table(table(imputada$PREOC)), col=palette("Tableau 10"),ylim=c(0,1), main = "PREOC imputada")
   text(0.75, 0.2305, "17.97%")
   text(1.9, 0.0675, "1.75%")
   text(3.15, 0.8520, "80.28%")
   legend("top", legend = levels(imputada$PREOC), fill = palette("Tableau 10"), cex=1)
   dev.off()
   
   # OPT
   freq(original$OPT, plot = FALSE, col = c("#0378FF","#6BB745"), main = "OPT original")
   png("OPT_original.png")
   pie (table(original$OPT), col = c("#0378FF","#6BB745"), main = "OPT (original)", labels = paste0(c("81.52", "18.48"), "%"))
   legend("topright", legend = levels(original$OPT), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   freq(imputada$OPT, plot = FALSE, col = c("#0378FF","#6BB745"), main = "OPT imputada")
   png("OPT_imputada.png")
   pie (table(imputada$OPT), col = c("#0378FF","#6BB745"), main = "OPT (imputada)", labels = paste0(c("81.09", "18.91"), "%"))
   legend("topright", legend = levels(imputada$OPT), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   # AFEC_P
   freq(original$AFEC_P, plot = FALSE, col = c("#0378FF","#6BB745"), main = "AFEC_P original")
   png("AFEC_P_original.png")
   pie (table(original$AFEC_P), col = c("#0378FF","#6BB745"), main = "AFEC_P (original)", labels = paste0(c("52.37", "47.63"), "%"))
   legend("topright", legend = levels(original$AFEC_P), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   freq(imputada$AFEC_P, plot = FALSE, col = c("#0378FF","#6BB745"), main = "AFEC_P
        imputada")
   png("AFEC_P_imputada.png")
   pie (table(imputada$AFEC_P), col = c("#0378FF","#6BB745"), main = "AFEC_P (imputada)", labels = paste0(c("52.37", "47.63"), "%"))
   legend("topright", legend = levels(imputada$AFEC_P), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   # COMPGEN
   freq(original$COMPGEN, plot = FALSE, col = c("#0378FF","#6BB745"), main = "COMPGEN original")
   png("COMPGEN_original.png")
   pie (table(original$COMPGEN), col = c("#0378FF","#6BB745"), main = "COMPGEN (original)", labels = paste0(c("80.4", "19.6"), "%"))
   legend("topright", legend = levels(original$COMPGEN), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   freq(imputada$COMPGEN, plot = FALSE, col = c("#0378FF","#6BB745"), main = "COMPGEN
        imputada")
   png("COMPGEN_imputada.png")
   pie (table(imputada$COMPGEN), col = c("#0378FF","#6BB745"), main = "COMPGEN (imputada)", labels = paste0(c("80.47", "19.53"), "%"))
   legend("topright", legend = levels(imputada$COMPGEN), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   # SINTOM
   freq(original$SINTOM, plot = FALSE, col = c("#0378FF","#6BB745"), main = "SINTOM original")
   png("SINTOM_original.png")
   pie (table(original$SINTOM), col = c("#0378FF","#6BB745"), main = "SINTOM (original)", labels = paste0(c("54.08", "45.92"), "%"))
   legend("topright", legend = levels(original$SINTOM), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()
   
   freq(imputada$SINTOM, plot = FALSE, col = c("#0378FF","#6BB745"), main = "SINTOM
        imputada")
   png("SINTOM_imputada.png")
   pie (table(imputada$SINTOM), col = c("#0378FF","#6BB745"), main = "SINTOM (imputada)", labels = paste0(c("54.04", "45.96"), "%"))
   legend("topright", legend = levels(imputada$SINTOM), fill = c("#0378FF","#6BB745"), cex=0.7)
   dev.off()

# Categoricas
actual <- original$OBLIG[is.na(nas$OBLIG)] 
predicted <- imputada$OBLIG[is.na(nas$OBLIG)] 
table(actual); table(predicted)

actual <- original$ESTADO_CIVIL[is.na(nas$ESTADO_CIVIL)] 
predicted <- imputada$ESTADO_CIVIL[is.na(nas$ESTADO_CIVIL)] 
table(actual); table(predicted)

actual <- original$ORIENT[is.na(nas$ORIENT)] 
predicted <- imputada$ORIENT[is.na(nas$ORIENT)] 
table(actual); table(predicted)

actual <- original$ESTUD[is.na(nas$ESTUD)] 
predicted <- imputada$ESTUD[is.na(nas$ESTUD)] 
table(actual); table(predicted)

actual <- original$RELIG[is.na(nas$RELIG)] 
predicted <- imputada$RELIG[is.na(nas$RELIG)] 
table(actual); table(predicted)

actual <- original$SIT_LAB[is.na(nas$SIT_LAB)] 
predicted <- imputada$SIT_LAB[is.na(nas$SIT_LAB)] 
table(actual); table(predicted)

actual <- original$SIT_ECON[is.na(nas$SIT_ECON)] 
predicted <- imputada$SIT_ECON[is.na(nas$SIT_ECON)] 
table(actual); table(predicted)

actual <- original$CLASE[is.na(nas$CLASE)] 
predicted <- imputada$CLASE[is.na(nas$CLASE)] 
table(actual); table(predicted)

actual <- original$PREOC[is.na(nas$PREOC)] 
predicted <- imputada$PREOC[is.na(nas$PREOC)] 
table(actual); table(predicted)

actual <- original$OPT[is.na(nas$OPT)] 
predicted <- imputada$OPT[is.na(nas$OPT)] 
table(actual); table(predicted)

actual <- original$AFEC_P[is.na(nas$AFEC_P)] 
predicted <- imputada$AFEC_P[is.na(nas$AFEC_P)] 
table(actual); table(predicted)

actual <- original$COMPGEN[is.na(nas$COMPGEN)] 
predicted <- imputada$COMPGEN[is.na(nas$COMPGEN)] 
table(actual); table(predicted)

actual <- original$SINTOM[is.na(nas$SINTOM)] 
predicted <- imputada$SINTOM[is.na(nas$SINTOM)] 
table(actual); table(predicted)


