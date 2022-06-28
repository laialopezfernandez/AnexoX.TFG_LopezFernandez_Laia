# ------------------------------------------------------------------------------
# Anexo 8: ESTIMACIÓN DE MODELOS (var. OBLIG)
# ------------------------------------------------------------------------------

# Carga de las librerías
library(easypackages)
paq <- c("effects", "car", "emmeans", "multcomp", "multcompView","ROCR", "pscl","mfx")
libraries(paq)


# Importación de las bases de datos
setwd("C:/Users/DATA00/Desktop/TFG Laia/2. Base de Dades/0.- Barometros/5. Barometros limpios")
Sept2021  <- as.data.frame(readRDS("Sept2021_imp.rds" , refhook = NULL));  
Feb2022   <- as.data.frame(readRDS("Feb2022_imp.rds"  , refhook = NULL));  

# Antes de nada agrupamos las categorias de TAMUNI en 4:
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
NS21 <- dim(Sept2021)[1] ; NF22 <- dim(Feb2022)[1]

set.seed(2022) # Semilla

i <- sample.int(NS21, size=(round(0.15*NS21, 0) + 1), replace=FALSE)
dd_e_S21 <- Sept2021[i,]   # Test data Sept21
dd_m_S21 <- Sept2021[-i,]  # Training data Sept21

i <- sample.int(NF22, size=(round(0.15*NF22, 0) + 1), replace=FALSE)
dd_e_F22 <- Feb2022[i,]   # Test data Feb22
dd_m_F22 <- Feb2022[-i,]  # Training data Feb22



# Barómetro Septiembre 2021
# ------------------------------------------------------------------------------
str(dd_m_S21) # La assignación de variables y factores es correcta

# Modelo 0: Todas las variables possibles
m.0 <- glm(OBLIG ~ CCAA + TAMUNI.b + SEXO + EDAD + ESTADO_CIVIL + EXTR + ORIENT +
                   ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                   AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                   AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL , 
           family = binomial, dd_m_S21)
summary(m.0)

# Model Nul
m.null <- glm(OBLIG ~ 1, family=binomial, data=dd_m_S21)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0, test="Chisq")


m.0.p2 <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + poly(EDAD,2) + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL , 
              family = binomial, data = dd_m_S21); summary(m.0.p2);

anova(m.0, m.0.p2, test="Chisq") # Termino cuadrado no significativo

m.0.p3 <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + poly(EDAD,3) + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL, 
              family = binomial, data = dd_m_S21); summary(m.0.p3);

anova(m.0.p2, m.0.p3, test="Chisq") # Termino cúbico significativo

m.0.p4 <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + poly(EDAD,4) + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL, 
              family = binomial, data = dd_m_S21); summary(m.0.p4);

anova(m.0.p3, m.0.p4, test="Chisq") # Termino a la cuarta no signidicativo

m.0.c <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + EDAD.c + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL, 
              family = binomial, data = dd_m_S21); summary(m.0.c);

# Model Nul
m.null <- glm(OBLIG ~ 1, family=binomial, data=dd_m_S21)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0.p3, test="Chisq")

# Posibles variables significativas: CCAA Navarra, Catalunya i Baleares, 
# EDAD, ESTADO_CIVIL, ORIENT, RELIG, CLASE, PREOC, AF_SALUD, COMPGEN Y DIAG

AIC(m.0.p3); AIC(m.0.c);
BIC(m.0.p3); BIC(m.0.c);
pR2(m.0.p3); pR2(m.0.c);

# STEP
step(m.0.p3, direction ="both", trace=FALSE, k = 2) # Algoritme millor model
step(m.0.c,  direction ="both", trace=FALSE, k = 2) # Algoritme millor model


m.1 <- glm(OBLIG ~ poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + CLASE + PREOC 
           + AF_SALUD + COMPGEN , family = binomial, dd_m_S21); summary(m.1)

m.2 <- glm(OBLIG ~ EDAD.c + ESTADO_CIVIL + ORIENT + RELIG + CLASE + PREOC +
             AF_SALUD + COMPGEN , family = binomial, dd_m_S21); summary(m.2)

AIC(m.1); AIC(m.2);
BIC(m.1); BIC(m.2);
pR2(m.1); pR2(m.2);


# Modelo 1. Como lo arreglamos?

# Validacion del modelo
# Gràfics residus
residualPlot(m.1)

# Autocorrelación
vif(m.1)

# La mayoria de VIF de las diferentes variables explicativas es cercano a 1, lo 
# que indicaria que no tenemos un problema de multicolinealidad elevada de las 
# variables explicativas.
# Las variables que tienen un VIF mayor son EDAD.c y ESTADO_CIVIL, estas tienen 
# Vif cercano a 2, lo que indica una correlación moderada entre estas variables
# predictoras y otras variables predictoras del modelo, en cualquier caso, este 
# valor no es lo suficientemente elevado para sugerir que tengamos un problema 
# importante.

# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/1. OBLIG/1. Sept 21")

png("EffectEDAD_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[1], ci.style="bands") # EDAD
dev.off()


png("EffectORIENT_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[2], ci.style="bars")  # ORIENT
dev.off()

png("EffectRELIG_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[3], ci.style="bars")  # RELIG
dev.off()

png("EffectCLASE_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[4], ci.style="bars")  # CLASE
dev.off()

png("EffectPREOC_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[5], ci.style="bars")  # PREOC
dev.off()

png("EffectAF_SALUD_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[6], ci.style="bars")  # AF_SALUD
dev.off()

png("EffectCOMPGEN_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[7], ci.style="bars")  # COMPGEN
dev.off()


# Comparaciones múltiples
png("emmeansESTADO_CIVIL_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ ESTADO_CIVIL,       type="response", rg.limit = 20000)))
dev.off()

png("emmeansORIENT_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ ORIENT,       type="response", rg.limit = 20000)))
dev.off()

png("emmeansRELIG_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ RELIG,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansCLASE_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ CLASE,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansPREOC_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ PREOC,        type="response", rg.limit = 20000)))
dev.off()

png("emmeansAF_SALUD_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_SALUD,     type="response", rg.limit = 20000)))
dev.off()

png("emmeansCOMPGEN_OBLIGSep21.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ COMPGEN,      type="response", rg.limit = 20000)))
dev.off()


## Model final
m.1_probit <- glm(OBLIG ~ poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + CLASE + PREOC 
                  + AF_SALUD + COMPGEN,  family = binomial(link=probit), 
                  data = dd_m_S21)

m.1_cloglog <- glm(OBLIG ~ poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + CLASE + PREOC 
                   + AF_SALUD + COMPGEN, family = binomial(link=cloglog), 
                   data = dd_m_S21)

logit <- summary(m.1); probit <- summary(m.1_probit); loglog <- summary(m.1_cloglog)

(AIC <- data.frame(logit$aic, probit$aic, loglog$aic))
(BIC <- c(BIC(m.1), BIC(m.1_probit), BIC(m.1_cloglog)))
pR2(m.1); pR2(m.1_probit); pR2(m.1_cloglog)

# Modelo final: Modelo 1 (función logit)
summary(m.1)

# Efectos
round(mfx::logitmfx(OBLIG ~ poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + CLASE 
                    + PREOC + AF_SALUD + COMPGEN, dd_m_S21)$mfxest, 6)



# Validacion del modelo

# Contraste de significación conjunta
anova(m.null, m.1, test="Chisq")

# Gràfics residus
png("ResidualPlot_OBLIGSep21.png", width=3000, height=2000, res=300)
   residualPlot(m.1)
dev.off()

# Autocorrelación
vif(m.1)

# La mayoria de VIF de las diferentes variables explicativas es cercano a 1, lo 
# que indicaria que no tenemos un problema de multicolinealidad elevada de las 
# variables explicativas.
# Las variables que tienen un VIF mayor son EDAD.c y ESTADO_CIVIL, estas tienen 
# Vif cercano a 2, lo que indica una correlación moderada entre estas variables
# predictoras y otras variables predictoras del modelo, en cualquier caso, este 
# valor no es lo suficientemente elevado para sugerir que tengamos un problema 
# importante.

# OUTLIER TEST
outlierTest(m.1)

png("influencePlot_OBLIGSep21.png", width=3000, height=2000, res=300)
   influenceIndexPlot(m.1,id=list(lab=row.names(dd_m_S21),vars=c("Cook", "Student","hat"), n=5))
dev.off()

influencePlot(m.1)

# pseudo R^2
pR2(m.1)

# Curva ROC
prob <- predict(m.1, dd_m_S21, ty="response")
prob <- as.data.frame(prob, nrow=length(prob), ncol=1)
prob <- cbind(prob, dd_m_S21$OBLIG)
colnames(prob)<- c("Prob","OBLIG")

pred <- prediction (prob$Prob, prob$OBLIG)
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
summary(Sept2021$OBLIG)

# Predictive capacity
ref <- 0.4817936

# Test data

table(dd_e_S21$OBLIG)
prob <- predict(m.1, dd_e_S21, ty="response")
est_e <- ifelse(prob < ref , 0, 1)
res <- table(est_e, dd_e_S21$OBLIG)
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
m.0 <- glm(OBLIG ~ CCAA + TAMUNI.b + SEXO + EDAD + ESTADO_CIVIL + EXTR + ORIENT +
             ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
             AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
             AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL , 
           family = binomial, dd_m_F22)
summary(m.0)

# Model Nul
m.null <- glm(OBLIG ~ 1, family=binomial, data=dd_m_F22)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0, test="Chisq")


m.0.p2 <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + poly(EDAD,2) + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL , 
              family = binomial, data = dd_m_F22); summary(m.0.p2);

anova(m.0, m.0.p2, test="Chisq") # Termino cuadrado no significativo

m.0.p3 <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + poly(EDAD,3) + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL, 
              family = binomial, data = dd_m_F22); summary(m.0.p3);

anova(m.0.p2, m.0.p3, test="Chisq") # Termino cúbico significativo

m.0.p4 <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + poly(EDAD,4) + ESTADO_CIVIL + EXTR + ORIENT +
                ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
                AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
                AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL, 
              family = binomial, data = dd_m_F22); summary(m.0.p4);

anova(m.0.p3, m.0.p4, test="Chisq") # Termino a la cuarta no signidicativo

m.0.c <- glm(OBLIG ~  CCAA + TAMUNI.b + SEXO + EDAD.c + ESTADO_CIVIL + EXTR + ORIENT +
               ESTUD + RELIG + SIT_LAB + SIT_ECON + CLASE + PREOC + OPT +
               AFEC_P + AF_ANIMICO + AF_SALUD + AF_ECON + AF_LIBERTAD + 
               AF_OTROS + COMPGEN + SINTOM + DIAG + EVOL, 
             family = binomial, data = dd_m_F22); summary(m.0.c);

# Model Nul
m.null <- glm(OBLIG ~ 1, family=binomial, data=dd_m_F22)

# El nostre model es significativament millor al model nul?
anova(m.null, m.0.p3, test="Chisq")

AIC(m.0.p3); AIC(m.0.c);
BIC(m.0.p3); BIC(m.0.c);
pR2(m.0.p3); pR2(m.0.c);

# STEP
step(m.0.p3, direction ="both", trace=FALSE, k = 2) # Algoritme millor model
step(m.0.c,  direction ="both", trace=FALSE, k = 2) # Algoritme millor model


m.1 <- glm(OBLIG ~ SEXO + poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + PREOC 
           + OPT + AFEC_P + AF_SALUD + COMPGEN , family = binomial, dd_m_F22); summary(m.1)

m.2 <- glm(OBLIG ~ SEXO + EDAD.c + ESTADO_CIVIL + ORIENT + RELIG  + PREOC + OPT +
             AFEC_P + AF_SALUD + COMPGEN , family = binomial, dd_m_F22); summary(m.2)

AIC(m.1); AIC(m.2);
BIC(m.1); BIC(m.2);
pR2(m.1); pR2(m.2);


# Modelo 1. Como lo arreglamos?

# Validacion del modelo
# Gràfics residus
residualPlot(m.1)

# Autocorrelación
vif(m.1)

# La mayoria de VIF de las diferentes variables explicativas es cercano a 1, lo 
# que indicaria que no tenemos un problema de multicolinealidad elevada de las 
# variables explicativas.
# Las variables que tienen un VIF mayor son EDAD.c y ESTADO_CIVIL, estas tienen 
# Vif cercano a 2, lo que indica una correlación moderada entre estas variables
# predictoras y otras variables predictoras del modelo, en cualquier caso, este 
# valor no es lo suficientemente elevado para sugerir que tengamos un problema 
# importante.

# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/1. OBLIG/2. Feb 22")

png("EffectSEXO_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[1], ci.style="bars") # SEXO
dev.off()

png("EffectEDAD_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[2], ci.style="bands") # EDAD
dev.off()

png("EffectESTADOCIVIL_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[3], ci.style="bars")  # ESTADO_CIVIL
dev.off()

png("EffectORIENT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[4], ci.style="bars")  # ORIENT
dev.off()

png("EffectRELIG_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[5], ci.style="bars")  # RELIG
dev.off()

png("EffectPREOC_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[6], ci.style="bars")  # PREOC
dev.off()

png("EffectOPT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[7], ci.style="bars")  # OPT
dev.off()

png("EffectAFEC_P_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[8], ci.style="bars")  # AFEC_P
dev.off()

png("EffectAF_SALUD_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[9], ci.style="bars")  # AF_SALUD
dev.off()

png("EffectCOMPGEN_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1)[10], ci.style="bars")  # COMPGEN
dev.off()


# Comparaciones múltiples

png("emmeansSEXO_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ SEXO, type="response", )))
dev.off()

png("emmeansESTADOCIVIL_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ ESTADO_CIVIL, type="response")))
dev.off()

png("emmeansORIENT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ ORIENT,       type="response")))
dev.off()

png("emmeansRELIG_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ RELIG,        type="response")))
dev.off()

png("emmeansPREOC_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ PREOC,        type="response")))
dev.off()

png("emmeansOPT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ OPT,        type="response")))
dev.off()

png("emmeansAFEC_P_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AFEC_P,     type="response")))
dev.off()

png("emmeansAF_SALUD_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ AF_SALUD,     type="response")))
dev.off()

png("emmeansCOMPGEN_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1,~ COMPGEN,      type="response")))
dev.off()


## Model final
m.1_probit <- glm(OBLIG ~ SEXO + poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + PREOC 
                  + OPT + AFEC_P + AF_SALUD + COMPGEN , family = binomial(link=probit), 
                  data = dd_m_F22)

m.1_cloglog <- glm(OBLIG ~ SEXO + poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + PREOC 
                   + OPT + AFEC_P + AF_SALUD + COMPGEN , family = binomial(link=cloglog), 
                   data = dd_m_F22)

logit <- summary(m.1); probit <- summary(m.1_probit); loglog <- summary(m.1_cloglog)

(AIC <- data.frame(logit$aic, probit$aic, loglog$aic))
(BIC <- c(BIC(m.1), BIC(m.1_probit), BIC(m.1_cloglog)))
pR2(m.1); pR2(m.1_probit); pR2(m.1_cloglog)

# Modelo final: Modelo 1 (función PROBIT)
summary(m.1_probit)

# Efectos
round(mfx::probitmfx(OBLIG ~ SEXO + poly(EDAD, 3) + ESTADO_CIVIL + ORIENT + RELIG + PREOC 
                     + OPT + AFEC_P + AF_SALUD + COMPGEN, dd_m_F22)$mfxest, 6)


# Efectos de las variables explicativas
setwd("C:/Users/DATA00/Desktop/TFG Laia/5. Modelos/1. OBLIG/2. Feb 22")

png("EffectSEXO_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[1], ci.style="bars") # SEXO
dev.off()

png("EffectEDAD_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[2], ci.style="bands") # EDAD
dev.off()

png("EffectESTADOCIVIL_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[3], ci.style="bars")  # ESTADO_CIVIL
dev.off()

png("EffectORIENT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[4], ci.style="bars")  # ORIENT
dev.off()

png("EffectRELIG_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[5], ci.style="bars")  # RELIG
dev.off()

png("EffectPREOC_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[6], ci.style="bars")  # PREOC
dev.off()

png("EffectOPT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[7], ci.style="bars")  # OPT
dev.off()

png("EffectAFEC_P_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[8], ci.style="bars")  # AFEC_P
dev.off()

png("EffectAF_SALUD_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[9], ci.style="bars")  # AF_SALUD
dev.off()

png("EffectCOMPGEN_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(allEffects(m.1_probit)[10], ci.style="bars")  # COMPGEN
dev.off()


# Comparaciones múltiples

png("emmeansSEXO_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ SEXO, type="response", )))
dev.off()

png("emmeansESTADOCIVIL_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ ESTADO_CIVIL, type="response")))
dev.off()

png("emmeansORIENT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ ORIENT,       type="response")))
dev.off()

png("emmeansRELIG_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ RELIG,        type="response")))
dev.off()

png("emmeansPREOC_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ PREOC,        type="response")))
dev.off()

png("emmeansOPT_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ OPT,        type="response")))
dev.off()

png("emmeansAFEC_P_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ AFEC_P,     type="response")))
dev.off()

png("emmeansAF_SALUD_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ AF_SALUD,     type="response")))
dev.off()

png("emmeansCOMPGEN_OBLIGFeb22.png", width=3000, height=2000, res=300)
   plot(cld(emmeans(m.1_probit,~ COMPGEN,      type="response")))
dev.off()


# Validacion del modelo

# Contraste de significación conjunta
anova(m.null, m.1_probit, test="Chisq")

# Gràfics residus
png("ResidualPlot_OBLIGFeb22.png", width=3000, height=2000, res=300)
   residualPlot(m.1_probit)
dev.off()

# Autocorrelación
vif(m.1_probit)

# La mayoria de VIF de las diferentes variables explicativas es cercano a 1, lo 
# que indicaria que no tenemos un problema de multicolinealidad elevada de las 
# variables explicativas.
# Las variables que tienen un VIF mayor son EDAD.c y ESTADO_CIVIL, estas tienen 
# Vif cercano a 2, lo que indica una correlación moderada entre estas variables
# predictoras y otras variables predictoras del modelo, en cualquier caso, este 
# valor no es lo suficientemente elevado para sugerir que tengamos un problema 
# importante.

# OUTLIER TEST
outlierTest(m.1_probit)

png("influencePlot_OBLIGFeb22.png", width=3000, height=2000, res=300)
   influenceIndexPlot(m.1_probit,id=list(lab=row.names(dd_m_S21),vars=c("Cook", "Student","hat"), n=5))
dev.off()

influencePlot(m.1_probit)

# pseudo R^2
pR2(m.1_probit)

# Curva ROC
prob <- predict(m.1_probit, dd_m_F22, ty="response")
prob <- as.data.frame(prob, nrow=length(prob), ncol=1)
prob <- cbind(prob, dd_m_F22$OBLIG)
colnames(prob)<- c("Prob","OBLIG")

pred <- prediction (prob$Prob, prob$OBLIG)
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
summary(Feb2022$OBLIG)

# Predictive capacity
ref <- 0.5131745

# Test data
prob <- predict(m.1_probit, dd_e_F22, ty="response")
est_e <- ifelse(prob < ref , 0, 1)
res <- table(est_e, dd_e_F22$OBLIG)
res

Encert <- sum(diag(res))/sum(res) 
Sensibilitat <- res[2,2]/sum(res[,2]); Especifitat  <- res[1,1]/sum(res[,1]) 
Pred.positiu <- res[2,2]/sum(res[2,]); Pred.negatiu <- res[1,1]/sum(res[1,])   

Indicadors <- data.frame(Encert, Sensibilitat, Especifitat, Pred.positiu, Pred.negatiu)
Indicadors
