# ------------------------------------------------------------------------------
# Anexo 10: ESTIMACIÓN DE MODELOS (var. PERS_COVID)
# ------------------------------------------------------------------------------

# Carga de las librerías
library(easypackages)
paq <- c("effects", "car", "emmeans", "multcomp", "multcompView","ROCR", "pscl", "mfx")
libraries(paq)

# Importación de las bases de datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/5. Barometros limpios")
Abril2020 <- readRDS("Abril2020_imp.rds", refhook = NULL);
Enero2021 <- readRDS("Enero2021_imp.rds", refhook = NULL); 
Sept2021  <- readRDS("Sept2021_imp.rds" , refhook = NULL);  
Feb2022   <- readRDS("Feb2022_imp.rds"  , refhook = NULL); 

# Antes de nada agrupamos las categorias de TAMUNI en 4:
summary(Abril2020$TAMUNI)

for(j in 1:dim(Abril2020)[1]){
  if(Abril2020$TAMUNI[j]=="<2.000" | Abril2020$TAMUNI[j]=="(2.000, 10.000]")
  {Abril2020$TAMUNI.b[j]<-1}
  
  if(Abril2020$TAMUNI[j]=="(10.000, 50.000]" | Abril2020$TAMUNI[j]=="(50.000, 100.000]")
  {Abril2020$TAMUNI.b[j]<-2}
  
  if(Abril2020$TAMUNI[j]=="(400.000, 1.000.000]" | Abril2020$TAMUNI[j]==">1.000.000")
  {Abril2020$TAMUNI.b[j]<-4}
  
  if(Abril2020$TAMUNI[j]=="(100.000, 400.000]") {Abril2020$TAMUNI.b[j]<-3}
}

table(Abril2020$TAMUNI.b); table(Abril2020$TAMUNI);

Abril2020$TAMUNI.b <- factor(Abril2020$TAMUNI.b, 
                             labels = c("<10.000", "(10.000, 100.000]", 
                                        "(100.000, 400.000]", ">400.000") , 
                             order=TRUE)

summary(Enero2021$TAMUNI)

for(j in 1:dim(Enero2021)[1]){
  if(Enero2021$TAMUNI[j]=="<2.000" | Enero2021$TAMUNI[j]=="(2.000, 10.000]")
  {Enero2021$TAMUNI.b[j]<-1}
  
  if(Enero2021$TAMUNI[j]=="(10.000, 50.000]" | Enero2021$TAMUNI[j]=="(50.000, 100.000]")
  {Enero2021$TAMUNI.b[j]<-2}
  
  if(Enero2021$TAMUNI[j]=="(400.000, 1.000.000]" | Enero2021$TAMUNI[j]==">1.000.000")
  {Enero2021$TAMUNI.b[j]<-4}
  
  if(Enero2021$TAMUNI[j]=="(100.000, 400.000]") {Enero2021$TAMUNI.b[j]<-3}
}

table(Enero2021$TAMUNI.b); table(Enero2021$TAMUNI);

Enero2021$TAMUNI.b <- factor(Enero2021$TAMUNI.b, 
                             labels = c("<10.000", "(10.000, 100.000]", 
                                        "(100.000, 400.000]", ">400.000") , 
                             order=TRUE)

summary(Sept2021$TAMUNI)

for(j in 1:dim(Sept2021)[1]){
  if(Sept2021$TAMUNI[j]=="<2.000" | Sept2021$TAMUNI[j]=="(2.000, 10.000]")
  {Sept2021$TAMUNI.b[j]<-1}
  
  if(Sept2021$TAMUNI[j]=="(10.000, 50.000]" | Sept2021$TAMUNI[j]=="(50.000, 100.000]")
  {Sept2021$TAMUNI.b[j]<-2}
  
  if(Sept2021$TAMUNI[j]=="(400.000, 1.000.000]" | Sept2021$TAMUNI[j]==">1.000.000")
  {Sept2021$TAMUNI.b[j]<-4}
  
  if(Sept2021$TAMUNI[j]=="(100.000, 400.000]") {Sept2021$TAMUNI.b[j]<-3}
}

table(Sept2021$TAMUNI.b); table(Sept2021$TAMUNI);

Sept2021$TAMUNI.b <- factor(Sept2021$TAMUNI.b, 
                            labels = c("<10.000", "(10.000, 100.000]", 
                                       "(100.000, 400.000]", ">400.000") , 
                            order=TRUE)

summary(Feb2022$TAMUNI)

for(j in 1:dim(Feb2022)[1]){
  if(Feb2022$TAMUNI[j]=="<2.000" | Feb2022$TAMUNI[j]=="(2.000, 10.000]")
  {Feb2022$TAMUNI.b[j]<-1}
  
  if(Feb2022$TAMUNI[j]=="(10.000, 50.000]" | Feb2022$TAMUNI[j]=="(50.000, 100.000]")
  {Feb2022$TAMUNI.b[j]<-2}
  
  if(Feb2022$TAMUNI[j]=="(400.000, 1.000.000]" | Feb2022$TAMUNI[j]==">1.000.000")
  {Feb2022$TAMUNI.b[j]<-4}
  
  if(Feb2022$TAMUNI[j]=="(100.000, 400.000]") {Feb2022$TAMUNI.b[j]<-3}
}

table(Feb2022$TAMUNI.b); table(Feb2022$TAMUNI);

Feb2022$TAMUNI.b <- factor(Feb2022$TAMUNI.b, 
                           labels = c("<10.000", "(10.000, 100.000]", 
                                      "(100.000, 400.000]", ">400.000") , 
                           order=TRUE)



# Asignamos correctamente el tipo de variable
# ------------------------------------------------------------------------------
Abril2020$EDAD.c   <- factor(Abril2020$EDAD.c, ordered=F)
Abril2020$ORIENT   <- factor(Abril2020$ORIENT, ordered=F)
Abril2020$CLASE    <- factor(Abril2020$CLASE, ordered=F)
Abril2020$ESTUD    <- factor(Abril2020$ESTUD, ordered=F)
Abril2020$SIT_ECON <- factor(Abril2020$SIT_ECON, ordered=F)
Abril2020$PREOC    <- factor(Abril2020$PREOC, ordered=F)
Abril2020$TAMUNI.b <- factor(Abril2020$TAMUNI.b, ordered=F)

Enero2021$EDAD.c   <- factor(Enero2021$EDAD.c, ordered=F)
Enero2021$ORIENT   <- factor(Enero2021$ORIENT, ordered=F)
Enero2021$CLASE    <- factor(Enero2021$CLASE, ordered=F)
Enero2021$ESTUD    <- factor(Enero2021$ESTUD, ordered=F)
Enero2021$SIT_ECON <- factor(Enero2021$SIT_ECON, ordered=F)
Enero2021$PREOC    <- factor(Enero2021$PREOC, ordered=F)
Enero2021$EVOL     <- factor(Enero2021$EVOL, ordered=F)
Enero2021$TAMUNI.b <- factor(Enero2021$TAMUNI.b, ordered=F)

Sept2021$EDAD.c   <- factor(Sept2021$EDAD.c, ordered=F)
Sept2021$ORIENT   <- factor(Sept2021$ORIENT, ordered=F)
Sept2021$CLASE    <- factor(Sept2021$CLASE, ordered=F)
Sept2021$ESTUD    <- factor(Sept2021$ESTUD, ordered=F)
Sept2021$SIT_ECON <- factor(Sept2021$SIT_ECON, ordered=F)
Sept2021$PREOC    <- factor(Sept2021$PREOC, ordered=F)
Sept2021$EVOL     <- factor(Sept2021$EVOL, ordered=F)
Sept2021$TAMUNI.b <- factor(Sept2021$TAMUNI.b, ordered=F)

Feb2022$EDAD.c   <- factor(Feb2022$EDAD.c, ordered=F)
Feb2022$ORIENT   <- factor(Feb2022$ORIENT, ordered=F)
Feb2022$CLASE    <- factor(Feb2022$CLASE, ordered=F)
Feb2022$ESTUD    <- factor(Feb2022$ESTUD, ordered=F)
Feb2022$SIT_ECON <- factor(Feb2022$SIT_ECON, ordered=F)
Feb2022$PREOC    <- factor(Feb2022$PREOC, ordered=F)
Feb2022$EVOL     <- factor(Feb2022$EVOL, ordered=F)
Feb2022$TAMUNI.b <- factor(Feb2022$TAMUNI.b, ordered=F)


# División de la base de datos
# ------------------------------------------------------------------------------
NA20 <- dim(Abril2020)[1]; NE21 <- dim(Enero2021)[1]
NS21 <- dim(Sept2021)[1] ; NF22 <- dim(Feb2022)[1]

set.seed(2022) # Semilla
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


# Barómetro Abril 2020
# ------------------------------------------------------------------------------
str(dd_m_A20) # La assignación de variables y factores es correcta

# Modelo 0: Todas las variables possibles
m.0 <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD + ESTADO_CIVIL + EXTR + ORIENT +
             ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT + COMPGEN , 
           family = binomial, dd_m_A20)
summary(m.0)

m.0.p2 <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + poly(EDAD,2) + ESTADO_CIVIL + EXTR + ORIENT +
             ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT + COMPGEN , 
           family = binomial, dd_m_A20)
summary(m.0.p2)

m.0.p3 <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + poly(EDAD,3) + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT + COMPGEN , 
              family = binomial, dd_m_A20)
summary(m.0.p3)

# Var. significativas: CCAALaRioja, EDAD, ESTADO_CIVIL y SIT_ECON

# Model Nul
m.null <- glm(PERS_COVID ~ 1, family=binomial, data=dd_m_A20)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0, test="Chisq")

m.0.c <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD.c + ESTADO_CIVIL + EXTR + ORIENT +
               ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT + COMPGEN, 
             family = binomial, data = dd_m_A20); summary(m.0.c);

# El nostre model es significativament millor al model nul?
anova(m.null, m.0.c, test="Chisq")

# Posibles variables significativas: CCAA Navarra, Catalunya i Baleares, 
# EDAD, ESTADO_CIVIL, ORIENT, RELIG, CLASE, PREOC, AF_SALUD, COMPGEN Y DIAG

AIC(m.0); AIC(m.0.p3); AIC(m.0.c);
BIC(m.0); BIC(m.0.p3); BIC(m.0.c);
pR2(m.0); pR2(m.0.p3); pR2(m.0.c);

# STEP
step(m.0.p3, direction ="both", trace=FALSE, k = 2) # Algoritme millor model
step(m.0.c, direction ="both", trace=FALSE, k = 2) # Algoritme millor model


m.1 <- glm(PERS_COVID ~ poly(EDAD, 3) + ORIENT + RELIG + SIT_LAB + SIT_ECON , 
           family = binomial, dd_m_A20); summary(m.1)

m.2 <- glm(PERS_COVID ~ EDAD.c + ORIENT + RELIG + SIT_ECON , 
           family = binomial, dd_m_A20); summary(m.2)

AIC(m.1); AIC(m.2);
BIC(m.1); BIC(m.2);
pR2(m.1); pR2(m.2);


# Modelo 2. Como lo arreglamos?

# Validacion del modelo
# Gràfics residus
residualPlot(m.2)

# Autocorrelación
vif(m.2)

# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/3. PERS_COVID/1. Abril 20")

png("EffectEDAD_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[1], ci.style="bars") # EDAD.C
dev.off()

png("EffectORIENT_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[2], ci.style="bars")  # ORIENT
dev.off()

png("EffectRELIG_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[3], ci.style="bars")  # RELIG
dev.off()

png("EffectSITECON_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[4], ci.style="bars")  # SIT_ECON
dev.off()

# Comparaciones múltiples

png("emmeansEDAD_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ EDAD.c, type="response", rg.limit = 20000)))
dev.off()

png("emmeansORIENT_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ ORIENT,       type="response", rg.limit = 20000)))
dev.off()

png("emmeansRELIG_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ RELIG,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansSITECON_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ SIT_ECON,        type="response", rg.limit = 20000)))
dev.off()


## Model final: funciones de enlace

m.2_probit <- glm(PERS_COVID ~ EDAD.c + ORIENT + RELIG + SIT_ECON ,
                  family = binomial(link=probit), 
                  data = dd_m_A20)

m.2_cloglog <- glm(PERS_COVID ~ EDAD.c + ORIENT + RELIG + SIT_ECON ,
                   family = binomial(link=cloglog), 
                   data = dd_m_A20)

logit <- summary(m.2); probit <- summary(m.2_probit); loglog <- summary(m.2_cloglog)

(AIC <- data.frame(logit$aic, probit$aic, loglog$aic))
(BIC <- c(BIC(m.2), BIC(m.2_probit), BIC(m.2_cloglog)))
pR2(m.2); pR2(m.2_probit); pR2(m.2_cloglog)

# Modelo final: Modelo 1 (función logit)
summary(m.2_cloglog)

# Efectos
round(mfx::logitmfx(PERS_COVID ~ EDAD.c + ORIENT + RELIG + SIT_ECON,dd_m_A20)$mfxest, 3)


setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/3. PERS_COVID/1. Abril 20")

png("EffectEDAD_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_cloglog)[1], ci.style="bars") # EDAD.C
dev.off()

png("EffectORIENT_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_cloglog)[2], ci.style="bars")  # ORIENT
dev.off()

png("EffectRELIG_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_cloglog)[3], ci.style="bars")  # RELIG
dev.off()

png("EffectSITECON_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_cloglog)[4], ci.style="bars")  # SIT_ECON
dev.off()


# Comparaciones múltiples

png("emmeansEDAD_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_cloglog,~ EDAD.c, type="response", rg.limit = 20000)))
dev.off()

png("emmeansORIENT_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_cloglog,~ ORIENT,       type="response", rg.limit = 20000)))
dev.off()

png("emmeansRELIG_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_cloglog,~ RELIG,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansSITECON_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_cloglog,~ SIT_ECON,        type="response", rg.limit = 20000)))
dev.off()



# Validacion del modelo

# Contraste de significación conjunta
anova(m.null, m.2_cloglog, test="Chisq")

# Gràfics residus
png("ResidualPlot_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   residualPlot(m.2_cloglog)
dev.off()

# Autocorrelación
vif(m.2_cloglog)

# OUTLIER TEST
outlierTest(m.2_cloglog)

png("influencePlot_PERS_COVIDAbril20.png", width=3000, height=2000, res=300)
   influenceIndexPlot(m.2_cloglog,id=list(lab=row.names(dd_m_S21),vars=c("Cook", "Student","hat"), n=5))
dev.off()

influencePlot(m.2_cloglog)

# pseudo R^2
pR2(m.2_cloglog)

# Curva ROC
PERS <- predict(m.2_cloglog, dd_m_A20, ty="response")
PERS <- as.data.frame(PERS, nrow=length(PERS), ncol=1)
PERS <- cbind(PERS, dd_m_A20$PERS_COVID)
colnames(PERS)<- c("PERS","PERS_COVID")

pred <- prediction (PERS$PERS, PERS$PERS_COVID)
perf <- performance(pred, measure="tpr", x.measure="fpr")

plot(perf, colorize=TRUE, type="l") 
abline(a=0,b=1)

# Área bajo la curva
AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values

# Punto de corte óptimo
cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#coordenadas del punto de corte óptimo
x <- perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y <- perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]

points(x, y, pch=20, col="red")
cat("AUC:", AUCaltura[[1]]) 
cat("Punto de corte óptimo:",opt.cut)

# MATRIZ DE CONFUSION
## Confussion Matrix 
summary(Abril2020$PERS_COVID)

# Predictive capacity
ref <- 0.5028144

# Test data
PERS <- predict(m.2_cloglog, dd_e_A20, ty="response")
est_e <- ifelse(PERS < ref , 0, 1)
res <- table(est_e, dd_e_A20$PERS_COVID)
res

Encert <- sum(diag(res))/sum(res) 
Sensibilitat <- res[2,2]/sum(res[,2]); Especifitat  <- res[1,1]/sum(res[,1]) 
Pred.positiu <- res[2,2]/sum(res[2,]); Pred.negatiu <- res[1,1]/sum(res[1,])   

Indicadors <- data.frame(Encert, Sensibilitat, Especifitat, Pred.positiu, Pred.negatiu)
Indicadors



# Barómetro Enero 2021
# ------------------------------------------------------------------------------
str(dd_m_E21) # La assignación de variables y factores es correcta

# Modelo 0: Todas las variables possibles
m.0 <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD + ESTADO_CIVIL + EXTR + ORIENT +
             ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
             AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
             AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL , 
           family = binomial, dd_m_E21)
summary(m.0)


# Var. significativas: 

# Model Nul
m.null <- glm(PERS_COVID ~ 1, family=binomial, data=dd_m_E21)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0, test="Chisq")

m.0.c <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD.c + ESTADO_CIVIL + EXTR + ORIENT +
               ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
               AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
               AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL ,  
             family = binomial, data = dd_m_E21); summary(m.0.c);


# El nostre model es significativament millor al model nul?
anova(m.null, m.0.c, test="Chisq")

AIC(m.0); AIC(m.0.c);
BIC(m.0); BIC(m.0.c);
pR2(m.0); pR2(m.0.c);

# STEP
step(m.0,   direction ="both", trace=FALSE, k = 2) # Algoritme millor model
step(m.0.c, direction ="both", trace=FALSE, k = 2) # Algoritme millor model


m.1 <- glm(PERS_COVID ~ EXTR + ORIENT + SIT_LAB + SIT_ECON + 
             PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + ESTADO_CIVIL, 
           family = binomial, dd_m_E21); summary(m.1)

AIC(m.0); AIC(m.0.c); AIC(m.1);
BIC(m.0); BIC(m.0.c); BIC(m.1);
pR2(m.0); pR2(m.0.c); pR2(m.1);


# Modelo 1. Como lo arreglamos?

# Validacion del modelo
# Gràfics residus
residualPlot(m.1)

# Autocorrelación
vif(m.1)

# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/3. PERS_COVID/2. Enero 21")

png("EffectEXTR_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[1], ci.style="bars") # EXTR
dev.off()

png("EffectORIENT_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[2], ci.style="bars")  # ORIENT
dev.off()

png("EffectSITLAB_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[3], ci.style="bars")  # SIT_LAB
dev.off()

png("EffectSITECON_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[4], ci.style="bars")  # SIT_ECON
dev.off()

png("EffectPREOC_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[5], ci.style="bars")  # PREOC
dev.off()

png("EffectAFECP_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[6], ci.style="bars")  # AFEC_P
dev.off()

png("EffectAF_ANIMICO_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[7], ci.style="bars")  # AF_ANIMICO
dev.off()

png("EffectAF_SALUD_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[8], ci.style="bars")  # AF_SALUD
dev.off()

png("EffectAF_ECON_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[9], ci.style="bars")  # AF_ECON
dev.off()

png("EffectESTADOCIVIL_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[10], ci.style="bars")  # ESTADO_CIVIL
dev.off()

# Comparaciones múltiples

png("emmeansEXTR_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ EXTR, type="response", rg.limit = 30000)))
dev.off()

png("emmeansORIENT_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ ORIENT,       type="response", rg.limit = 30000)))
dev.off()

png("emmeansSIT_LAB_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ SIT_LAB,        type="response", rg.limit = 30000)))
dev.off()

png("emmeansSIT_ECON_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ SIT_ECON,        type="response", rg.limit = 30000)))
dev.off()

png("emmeansPREOC_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ PREOC,        type="response", rg.limit = 30000)))
dev.off()

png("emmeansAFEC_P_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AFEC_P,     type="response", rg.limit = 30000)))
dev.off()

png("emmeansAF_ANIMICO_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_ANIMICO,     type="response", rg.limit = 30000)))
dev.off()

png("emmeansAF_SALUD_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_SALUD,     type="response", rg.limit = 30000)))
dev.off()

png("emmeansAF_ECON_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_ECON,     type="response", rg.limit = 30000)))
dev.off()

png("emmeansESTADOCIVIL_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ ESTADO_CIVIL,     type="response", rg.limit = 30000)))
dev.off()


## Model final
m.1_probit <- glm(PERS_COVID ~ EXTR + ORIENT + SIT_LAB + SIT_ECON + 
                    PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + ESTADO_CIVIL,
                  family = binomial(link=probit), 
                  data = dd_m_E21)

m.1_cloglog <- glm(PERS_COVID ~ EXTR + ORIENT + SIT_LAB + SIT_ECON + 
                     PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + ESTADO_CIVIL,
                   family = binomial(link=cloglog), 
                   data = dd_m_E21)

logit <- summary(m.1); probit <- summary(m.1_probit); loglog <- summary(m.1_cloglog)

(AIC <- data.frame(logit$aic, probit$aic, loglog$aic))
(BIC <- c(BIC(m.1), BIC(m.1_probit), BIC(m.1_cloglog)))
pR2(m.2); pR2(m.2_probit); pR2(m.2_cloglog)


# Modelo final: Modelo 1 (función logit)
summary(m.1)

# Efectos
round(mfx::logitmfx(PERS_COVID ~ EXTR + ORIENT + SIT_LAB + SIT_ECON + 
                      PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + ESTADO_CIVIL,
                    dd_m_E21)$mfxest, 6)


# Validacion del modelo

# Contraste de significación conjunta
anova(m.null, m.1, test="Chisq")

# Gràfics residus
png("ResidualPlot_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   residualPlot(m.1)
dev.off()

# Autocorrelación
vif(m.1)

# OUTLIER TEST
outlierTest(m.1)

png("influencePlot_PERS_COVIDEnero21.png", width=3000, height=2000, res=300)
   influenceIndexPlot(m.1,id=list(lab=row.names(dd_m_S21),vars=c("Cook", "Student","hat"), n=5))
dev.off()

influencePlot(m.1)

# pseudo R^2
pR2(m.1)

# Curva ROC
PERS <- predict(m.1, dd_m_E21, ty="response")
PERS <- as.data.frame(PERS, nrow=length(PERS), ncol=1)
PERS <- cbind(PERS, dd_m_E21$PERS_COVID)
colnames(PERS)<- c("PERS","PERS_COVID")

pred <- prediction (PERS$PERS, PERS$PERS_COVID)
perf <- performance(pred, measure="tpr", x.measure="fpr")

plot(perf, colorize=TRUE, type="l") 
abline(a=0,b=1)

# Área bajo la curva
AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values

# Punto de corte óptimo
cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#coordenadas del punto de corte óptimo
x <- perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y <- perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]

points(x, y, pch=20, col="red")
cat("AUC:", AUCaltura[[1]]) 
cat("Punto de corte óptimo:",opt.cut)

# MATRIZ DE CONFUSION
## Confussion Matrix 
summary(Enero2021$PERS_COVID)

# Predictive capacity
ref <- 0.5061167

# Test data
PERS <- predict(m.1, dd_e_E21, ty="response")
est_e <- ifelse(PERS < ref , 0, 1)
res <- table(est_e, dd_e_E21$PERS_COVID)
res

Encert <- sum(diag(res))/sum(res) 
Sensibilitat <- res[2,2]/sum(res[,2]); Especifitat  <- res[1,1]/sum(res[,1]) 
Pred.positiu <- res[2,2]/sum(res[2,]); Pred.negatiu <- res[1,1]/sum(res[1,])   

Indicadors <- data.frame(Encert, Sensibilitat, Especifitat, Pred.positiu, Pred.negatiu)
Indicadors


# Barómetro Septiembre 2021
# ------------------------------------------------------------------------------
str(dd_m_S21) # La assignación de variables y factores es correcta

# Modelo 0: Todas las variables possibles
m.0 <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD + ESTADO_CIVIL + EXTR + ORIENT +
             ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
             AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
             AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL , 
           family = binomial, dd_m_S21)
summary(m.0)


# Model Nul
m.null <- glm(PERS_COVID ~ 1, family=binomial, data=dd_m_S21)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0, test="Chisq")

m.0.c <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD.c + ESTADO_CIVIL + EXTR + ORIENT +
               ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
               AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
               AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL ,  
             family = binomial, data = dd_m_S21); summary(m.0.c);

# El nostre model es significativament millor al model nul?
anova(m.null, m.0.c, test="Chisq")


AIC(m.0); AIC(m.0.c);
BIC(m.0); BIC(m.0.c);
pR2(m.0); pR2(m.0.c);

# STEP
step(m.0,   direction ="both", trace=FALSE, k = 2) # Algoritme millor model
step(m.0.c, direction ="both", trace=FALSE, k = 2) # Algoritme millor model


m.1 <- glm(PERS_COVID ~ ORIENT + RELIG + SIT_LAB + SIT_ECON + 
             PREOC + AFEC_P + AF_ANIMICO + AF_ECON + AF_LIBERTAD + AF_OTROS, 
           family = binomial, dd_m_S21); summary(m.1)

AIC(m.0); AIC(m.0.c); AIC(m.1);
BIC(m.0); BIC(m.0.c); BIC(m.1);
pR2(m.0); pR2(m.0.c); pR2(m.1);


# Modelo 1. Como lo arreglamos?

# Validacion del modelo
# Gràfics residus
residualPlot(m.1)

# Autocorrelación
vif(m.1)

# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/3. PERS_COVID/3. Sept 21")

png("EffectORIENT_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[1], ci.style="bars") # ORIENT
dev.off()

png("EffectRELIG_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[2], ci.style="bars")  # RELIG
dev.off()

png("EffectSIT_LAB_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[3], ci.style="bars")  # SIT_LAB
dev.off()

png("EffectSIT_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[4], ci.style="bars")  # SIT_ECON
dev.off()

png("EffectPREOC_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[5], ci.style="bars")  # PREOC
dev.off()

png("EffectAFEC_P_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[6], ci.style="bars")  # AFEC_P
dev.off()

png("EffectAF_ANIMICO_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[7], ci.style="bars")  # AF_ANIMICO
dev.off()

png("EffectAF_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[8], ci.style="bars")  # AF_ECON
dev.off()

png("EffectAF_LIBERTAD_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[9], ci.style="bars")  # AF_LIBERTAD
dev.off()

png("EffectAF_LIBERTAD_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[10], ci.style="bars")  # AF_OTROS
dev.off()

# Comparaciones múltiples

png("emmeansORIENT_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ ORIENT,       type="response", rg.limit = 25000)))
dev.off()

png("emmeansRELIG_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ RELIG, type="response", rg.limit = 25000)))
dev.off()

png("emmeansSIT_LAB_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ SIT_LAB,        type="response", rg.limit = 25000)))
dev.off()

png("emmeansSIT_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ SIT_ECON,        type="response", rg.limit = 25000)))
dev.off()


png("emmeansPREOC_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ PREOC,        type="response", rg.limit = 25000)))
dev.off()

png("emmeansAFEC_P_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AFEC_P,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_ANIMICO_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_ANIMICO,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_ECON,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_LIBERTAD_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_LIBERTAD,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_OTROS_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_OTROS,     type="response", rg.limit = 25000)))
dev.off()


## Model final
m.1_probit <- glm(PERS_COVID ~ ORIENT + RELIG + SIT_LAB + SIT_ECON + 
                    PREOC + AFEC_P + AF_ANIMICO + AF_ECON + AF_LIBERTAD + AF_OTROS,
                  family = binomial(link=probit), 
                  data = dd_m_S21)

m.1_cloglog <- glm(PERS_COVID ~ ORIENT + RELIG + SIT_LAB + SIT_ECON + 
                     PREOC + AFEC_P + AF_ANIMICO + AF_ECON + AF_LIBERTAD + AF_OTROS,
                   family = binomial(link=cloglog), 
                   data = dd_m_S21)

logit <- summary(m.1); probit <- summary(m.1_probit); loglog <- summary(m.1_cloglog)

(AIC <- data.frame(logit$aic, probit$aic, loglog$aic))
(BIC <- c(BIC(m.2), BIC(m.2_probit), BIC(m.2_cloglog)))
pR2(m.2); pR2(m.2_probit); pR2(m.2_cloglog)

# Modelo final: Modelo 1 (función probit)
summary(m.1_probit)

# Efectos
round(mfx::probitmfx(PERS_COVID ~ ORIENT + RELIG + SIT_LAB + SIT_ECON + 
                       PREOC + AFEC_P + AF_ANIMICO + AF_ECON + AF_LIBERTAD + AF_OTROS, dd_m_S21)$mfxest, 6)


# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/3. PERS_COVID/3. Sept 21")

png("EffectORIENT_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[1], ci.style="bars") # ORIENT
dev.off()

png("EffectRELIG_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[2], ci.style="bars")  # RELIG
dev.off()

png("EffectSIT_LAB_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[3], ci.style="bars")  # SIT_LAB
dev.off()

png("EffectSIT_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[4], ci.style="bars")  # SIT_ECON
dev.off()

png("EffectPREOC_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[5], ci.style="bars")  # PREOC
dev.off()

png("EffectAFEC_P_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[6], ci.style="bars")  # AFEC_P
dev.off()

png("EffectAF_ANIMICO_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[7], ci.style="bars")  # AF_ANIMICO
dev.off()

png("EffectAF_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[8], ci.style="bars")  # AF_ECON
dev.off()

png("EffectAF_LIBERTAD_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[9], ci.style="bars")  # AF_LIBERTAD
dev.off()

png("EffectAF_OTROS_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[10], ci.style="bars")  # AF_OTROS
dev.off()

# Comparaciones múltiples

png("emmeansORIENT_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ ORIENT,       type="response", rg.limit = 25000)))
dev.off()

png("emmeansRELIG_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ RELIG, type="response", rg.limit = 25000)))
dev.off()

png("emmeansSIT_LAB_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ SIT_LAB,        type="response", rg.limit = 25000)))
dev.off()

png("emmeansSIT_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ SIT_ECON,        type="response", rg.limit = 25000)))
dev.off()

png("emmeansPREOC_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ PREOC,        type="response", rg.limit = 25000)))
dev.off()

png("emmeansAFEC_P_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ AFEC_P,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_ANIMICO_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ AF_ANIMICO,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_ECON_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ AF_ECON,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_LIBERTAD_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ AF_LIBERTAD,     type="response", rg.limit = 25000)))
dev.off()

png("emmeansAF_OTROS_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ AF_OTROS,     type="response", rg.limit = 25000)))
dev.off()




# Validacion del modelo

# Contraste de significación conjunta
anova(m.null, m.1_probit, test="Chisq")

# Gràfics residus
png("ResidualPlot_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   residualPlot(m.1_probit)
dev.off()

# Autocorrelación
vif(m.1_probit)

# OUTLIER TEST
outlierTest(m.1_probit)

png("influencePlot_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   influenceIndexPlot(m.1_probit,id=list(lab=row.names(dd_m_S21),vars=c("Cook", "Student","hat"), n=5))
dev.off()

influencePlot(m.1_probit)

# pseudo R^2
pR2(m.1_probit)

# Curva ROC
PERS <- predict(m.1_probit, dd_m_S21, ty="response")
PERS <- as.data.frame(PERS, nrow=length(PERS), ncol=1)
PERS <- cbind(PERS, dd_m_S21$PERS_COVID)
colnames(PERS)<- c("PERS","PERS_COVID")

pred <- prediction (PERS$PERS, PERS$PERS_COVID)
perf <- performance(pred, measure="tpr", x.measure="fpr")

plot(perf, colorize=TRUE, type="l") 
abline(a=0,b=1)

# Área bajo la curva
AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values

# Punto de corte óptimo
cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#coordenadas del punto de corte óptimo
x <- perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y <- perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]

points(x, y, pch=20, col="red")
cat("AUC:", AUCaltura[[1]]) 
cat("Punto de corte óptimo:",opt.cut)

# MATRIZ DE CONFUSION
## Confussion Matrix 
summary(Sept2021$PERS_COVID)

# Predictive capacity
ref <- 0.2539002

# Test data
PERS <- predict(m.1_probit, dd_e_S21, ty="response")
est_e <- ifelse(PERS < ref , 0, 1)
res <- table(est_e, dd_e_S21$PERS_COVID)
res

Encert <- sum(diag(res))/sum(res) 
Sensibilitat <- res[2,2]/sum(res[,2]); Especifitat  <- res[1,1]/sum(res[,1]) 
Pred.positiu <- res[2,2]/sum(res[2,]); Pred.negatiu <- res[1,1]/sum(res[1,])   

Indicadors <- data.frame(Encert, Sensibilitat, Especifitat, Pred.positiu, Pred.negatiu)
Indicadors



# Barómetro Febrero 2022
# ------------------------------------------------------------------------------
str(dd_m_F22) # La assignación de variables y factores es correcta

# Modelo 0: Todas las variables possibles
m.0 <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD + ESTADO_CIVIL + EXTR + ORIENT +
             ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
             AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
             AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL , 
           family = binomial, dd_m_F22)
summary(m.0)


# Model Nul
m.null <- glm(PERS_COVID ~ 1, family=binomial, data=dd_m_F22)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0, test="Chisq")

m.0.c <- glm(PERS_COVID ~ CCAA + TAMUNI.b + SEXO + EDAD.c + ESTADO_CIVIL + EXTR + ORIENT +
               ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
               AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
               AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL ,  
             family = binomial, data = dd_m_F22); summary(m.0.c);

# El nostre model es significativament millor al model nul?
anova(m.null, m.0.c, test="Chisq")


AIC(m.0); AIC(m.0.c);
BIC(m.0); BIC(m.0.c);
pR2(m.0); pR2(m.0.c);

# STEP
step(m.0, direction ="both", trace=FALSE, k = 2) # Algoritme millor model
step(m.0.c, direction ="both", trace=FALSE, k = 2) # Algoritme millor model


m.1 <- glm(PERS_COVID ~ SEXO + ORIENT + SIT_LAB + SIT_ECON + 
             PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + COMPGEN,
           family = binomial, dd_m_F22); summary(m.1)

m.2 <- glm(PERS_COVID ~ SEXO + EDAD.c + ORIENT + SIT_ECON + 
             PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + COMPGEN, 
           family = binomial, dd_m_F22); summary(m.2)

AIC(m.1); AIC(m.2);
BIC(m.1); BIC(m.2);
pR2(m.1); pR2(m.2);


# Modelo 2. Como lo arreglamos?

# Validacion del modelo
# Gràfics residus
residualPlot(m.2)

# Autocorrelación
vif(m.2)

# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/3. PERS_COVID/4. Feb 22")

png("EffectSEXO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[1], ci.style="bars") # SEXO
dev.off()

png("EffectEDAD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[2], ci.style="bars")  # EDAD
dev.off()

png("EffectORIENT_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[3], ci.style="bars")  # ORIENT
dev.off()

png("EffectSIT_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[4], ci.style="bars")  # SIT_ECON
dev.off()

png("EffectPREOC_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[5], ci.style="bars")  # PREOC
dev.off()

png("EffectAFEC_P_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[6], ci.style="bars")  # AFEC_P
dev.off()

png("EffectAF_ANIMICO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[7], ci.style="bars")  # AF_ANIMICO
dev.off()

png("EffectAF_SALUD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[8], ci.style="bars")  # AF_SALUD
dev.off()

png("EffectAF_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[9], ci.style="bars")  # AF_ECON
dev.off()

png("EffectCOMPGEN_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2)[10], ci.style="bars")  # COMPGEN
dev.off()


# Comparaciones múltiples

png("emmeansSEXO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ SEXO, type="response", rg.limit = 20000)))
dev.off()

png("emmeansEDAD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ EDAD.c, type="response", rg.limit = 20000)))
dev.off()

png("emmeansORIENT_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ ORIENT,       type="response", rg.limit = 20000)))
dev.off()

png("emmeansSIT_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ SIT_ECON,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansPREOC_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ PREOC,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansAFEC_P_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ AFEC_P,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansAF_ANIMICO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ AF_ANIMICO,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansAF_SALUD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ AF_SALUD,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansAF_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ AF_ECON,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansCOMPGEN_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2,~ COMPGEN,     type="response", rg.limit = 20000)))
dev.off()


## Model final
m.2_probit <- glm(PERS_COVID ~ SEXO + EDAD.c + ORIENT + SIT_ECON + 
                    PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + COMPGEN, 
                  family = binomial(link=probit), 
                  data = dd_m_F22)

m.2_cloglog <- glm(PERS_COVID ~ SEXO + EDAD.c + ORIENT + SIT_ECON + 
                     PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + COMPGEN, 
                   family = binomial(link=cloglog), 
                   data = dd_m_F22)

logit <- summary(m.2); probit <- summary(m.2_probit); loglog <- summary(m.2_cloglog)

(AIC <- data.frame(logit$aic, probit$aic, loglog$aic))
(BIC <- c(BIC(m.2), BIC(m.2_probit), BIC(m.2_cloglog)))
pR2(m.2); pR2(m.2_probit); pR2(m.2_cloglog)

# Modelo final: Modelo 1 (función probit)
summary(m.2_probit)

# Efectos
round(mfx::probitmfx(PERS_COVID ~ SEXO + EDAD.c + ORIENT + SIT_ECON + 
                       PREOC + AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + COMPGEN, dd_m_F22)$mfxest,6)


# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/3. PERS_COVID/4. Feb 22")

png("EffectSEXO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[1], ci.style="bars") # SEXO
dev.off()

png("EffectEDAD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[2], ci.style="bars")  # EDAD
dev.off()

png("EffectORIENT_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[3], ci.style="bars")  # ORIENT
dev.off()

png("EffectSIT_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[4], ci.style="bars")  # SIT_ECON
dev.off()

png("EffectPREOC_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[5], ci.style="bars")  # PREOC
dev.off()

png("EffectAFEC_P_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[6], ci.style="bars")  # AFEC_P
dev.off()

png("EffectAF_ANIMICO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[7], ci.style="bars")  # AF_ANIMICO
dev.off()

png("EffectAF_SALUD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[8], ci.style="bars")  # AF_SALUD
dev.off()

png("EffectAF_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[9], ci.style="bars")  # AF_ECON
dev.off()

png("EffectCOMPGEN_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.2_probit)[10], ci.style="bars")  # COMPGEN
dev.off()


# Comparaciones múltiples

png("emmeansSEXO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ SEXO, type="response", rg.limit = 20000)))
dev.off()

png("emmeansEDAD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ EDAD.c, type="response", rg.limit = 20000)))
dev.off()

png("emmeansORIENT_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ ORIENT,       type="response", rg.limit = 20000)))
dev.off()

png("emmeansSIT_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ SIT_ECON,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansPREOC_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ PREOC,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansAFEC_P_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ AFEC_P,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansAF_ANIMICO_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ AF_ANIMICO,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansAF_SALUD_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ AF_SALUD,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansAF_ECON_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ AF_ECON,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansCOMPGEN_PERS_COVIDFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.2_probit,~ COMPGEN,     type="response", rg.limit = 20000)))
dev.off()



# Validacion del modelo

# Contraste de significación conjunta
anova(m.null, m.2_probit, test="Chisq")

# Gràfics residus
png("ResidualPlot_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   residualPlot(m.2_probit)
dev.off()

# Autocorrelación
vif(m.2_probit)

# OUTLIER TEST
outlierTest(m.2_probit)

png("influencePlot_PERS_COVIDSept21.png", width=3000, height=2000, res=300)
   influenceIndexPlot(m.2_probit,id=list(lab=row.names(dd_m_S21),vars=c("Cook", "Student","hat"), n=5))
dev.off()

influencePlot(m.2_probit)

# pseudo R^2
pR2(m.2_probit)

# Curva ROC
PERS <- predict(m.2_probit, dd_m_F22, ty="response")
PERS <- as.data.frame(PERS, nrow=length(PERS), ncol=1)
PERS <- cbind(PERS, dd_m_F22$PERS_COVID)
colnames(PERS)<- c("PERS","PERS_COVID")

pred <- prediction (PERS$PERS, PERS$PERS_COVID)
perf <- performance(pred, measure="tpr", x.measure="fpr")

plot(perf, colorize=TRUE, type="l") 
abline(a=0,b=1)

# Área bajo la curva
AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values

# Punto de corte óptimo
cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#coordenadas del punto de corte óptimo
x <- perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
y <- perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]

points(x, y, pch=20, col="red")
cat("AUC:", AUCaltura[[1]]) 
cat("Punto de corte óptimo:",opt.cut)

# MATRIZ DE CONFUSION
## Confussion Matrix 
summary(Feb2022$PERS_COVID)

# Predictive capacity
ref <- summary(Feb2022$PERS_COVID)[2] / (summary(Feb2022$PERS_COVID)[1] + summary(Feb2022$PERS_COVID)[2])

# Test data
PERS <- predict(m.2_probit, dd_e_F22, ty="response")
est_e <- ifelse(PERS < ref , 0, 1)
res <- table(est_e, dd_e_F22$PERS_COVID)
res

Encert <- sum(diag(res))/sum(res) 
Sensibilitat <- res[2,2]/sum(res[,2]); Especifitat  <- res[1,1]/sum(res[,1]) 
Pred.positiu <- res[2,2]/sum(res[2,]); Pred.negatiu <- res[1,1]/sum(res[1,])   

Indicadors <- data.frame(Encert, Sensibilitat, Especifitat, Pred.positiu, Pred.negatiu)
Indicadors
