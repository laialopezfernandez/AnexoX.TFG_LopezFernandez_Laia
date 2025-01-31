# ------------------------------------------------------------------------------
# Anexo 2: Gr�ficos de la evoluci�n de la vacunaci�n contra la Covid-19 en
# Espa�a
# ------------------------------------------------------------------------------

# Carga de las librer�as
library(easypackages)
paq <-  c("readxl "); libraries (paq)

# Importaci�n de la Base de Datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/4. Gr�ficos/Bases de datos")
Primeradosis  <- read_excel("Vacunacion_evol.xlsx", sheet = 1)
Pautacompleta <- read_excel("Vacunacion_evol.xlsx", sheet = 2)

# Gr�fico 1: Primera dosis
# ------------------------------------------------------------------------------
Primeradosis$Dia <- as.Date(Primeradosis$Dia)

# agregue espacio adicional a la derecha del par de parcela
par( mar = c (2, 2, 3, 2), xpd = TRUE )

plot(Primeradosis$Dia, Primeradosis$`M�s de 80`, type = "l", col = 2,
     ylim = c(0, 1), xlab = "Dia", ylab = "% vacunaci�n", xaxt = "n",
     main= "Vacunaci�n en Espa�a (Marzo 2021 a Mayo 2022)")

axis(1, at = seq(Primeradosis$Dia[1], Primeradosis$Dia[57],  
                 along = Primeradosis$Dia), labels = format(Primeradosis$Dia, "%Y-%m"))


lines(Primeradosis$Dia, Primeradosis$`70 a 79`, type = "l", col = 3)
lines(Primeradosis$Dia, Primeradosis$`60 a 69`, type = "l", col = 4)
lines(Primeradosis$Dia, Primeradosis$`50 a 59`, type = "l", col = 5)
lines(Primeradosis$Dia, Primeradosis$`20 a 49`, type = "l", col = 6)
lines(Primeradosis$Dia, Primeradosis$`12 a 19`, type = "l", col = 7)
lines(Primeradosis$Dia, Primeradosis$`5 a 12`,  type = "l", col = 8)

legend(x= "right", inset = c(0, -3), c("M�s de 80", "70-79", "60-69", "50-59", 
                                       "20-49", "12-19", "5-12"), lty = 1, col = 2:8, lwd = 1, xpd = TRUE)


# Gr�fico 1: Pauta completa
# ------------------------------------------------------------------------------
Pautacompleta$Dia <- as.Date(Pautacompleta$Dia)

# agregue espacio adicional a la derecha del par de parcela
par( mar = c (2, 2, 3, 2), xpd = TRUE )

plot(Pautacompleta$Dia, Pautacompleta$`M�s de 80`, type = "l", col = 2,
     ylim = c(0, 1), xlab = "Dia", ylab = "% vacunaci�n", xaxt = "n",
     main= "Vacunaci�n en Espa�a (Marzo 2021 a Mayo 2022)")

axis(1, at = seq(Pautacompleta$Dia[1], Pautacompleta$Dia[57],  
                 along = Pautacompleta$Dia), labels = format(Pautacompleta$Dia, "%Y-%m"))


lines(Pautacompleta$Dia, Pautacompleta$`70 a 79`, type = "l", col = 3)
lines(Pautacompleta$Dia, Pautacompleta$`60 a 69`, type = "l", col = 4)
lines(Pautacompleta$Dia, Pautacompleta$`50 a 59`, type = "l", col = 5)
lines(Pautacompleta$Dia, Pautacompleta$`20 a 49`, type = "l", col = 6)
lines(Pautacompleta$Dia, Pautacompleta$`12 a 19`, type = "l", col = 7)
lines(Pautacompleta$Dia, Pautacompleta$`5 a 12`,  type = "l", col = 8)

legend(x= "right", inset = c(0, -3), c("M�s de 80", "70-79", "60-69", "50-59", 
                                       "20-49", "12-19", "5-12"), lty = 1, col = 2:8, lwd = 1, xpd = TRUE)
