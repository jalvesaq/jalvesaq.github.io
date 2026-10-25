# Código para replicar resultados de:
# GOMES, Marcio de Lucas; AQUINO, Jakson alves de. Violência e satisfação com
# a democracia no Brasil. Opinião Pública, v. 24, p. 209-238, 2018.

library(foreign)
library(descr)
library(lavaan)
library(mediation)
library(cem)
library(mgcv)
library(randomForest)

am <- read.spss("863896541Brazil LAPOP AmericasBarometer 2014 Espanol v3.0_W.sav",
                reencode = TRUE, to.data.frame = TRUE)

am$sexo <- as.numeric(am$q1 == "Mujer")

am$idade <- 2014 - as.integer(as.character(am$q2y))

am$cor <- as.numeric(am$etid == "Blanco")

am$escola <- as.character(am$ed)
am$escola[am$escola == "Ninguno"] <- "0"
am$escola <- as.numeric(am$escola)

am$total_income <- as.numeric(am$q10new) - 1

am$renda <- am$total_income / as.integer(as.character(am$q12c))

am$conf <- 4 - as.numeric(am$it1)

am$vitima <- as.numeric(am$vic1ext == "Sí")

am$segur <- 4 - as.numeric(am$aoj11)

am$judiciary <- 4 - as.numeric(am$aoj12)

am$police <- 4 - as.numeric(am$pole2n)

am$punitiv <- am$judiciary + am$police

am$satisfdem <- 4 - as.numeric(am$pn4)

am <- am[is.na(am$conf) == FALSE &
         is.na(am$vitima) == FALSE &
         is.na(am$segur) == FALSE &
         is.na(am$punitiv) == FALSE &
         is.na(am$satisfdem) == FALSE, ]

am <- am[, c('sexo', 'idade', 'cor', 'escola', 'renda', 'conf', 'vitima',
             'segur', 'punitiv', 'satisfdem')]

### Completando valores omissos
for(v in c('cor', 'escola', 'renda')){
    x <- am[, v]
    x[is.na(x)] <- sample(x[!is.na(x)], sum(is.na(x)), replace = TRUE)
    am[, v] <- x
}

### Modelos de Regressão

ols1 <- lm(satisfdem ~ conf + segur + punitiv + vitima +
           renda + escola + cor + idade + sexo, data = am)

summary(ols1)

ols2 <- lm(conf ~ segur + punitiv + vitima + renda +
           escola + cor + idade + sexo, data = am)

summary(ols2)

### Modelo de Equações Simultâneas

model <- 'satisfdem ~ b1*conf + b2*punitiv + b3*segur + b4*vitima + cor + idade + renda + escola + sexo

          conf ~ c1*punitiv + c2*segur + c3*vitima + cor + idade + renda + escola + sexo

          ef_direto := b2 + b3 + b4

          ef_indireto := b1*(c1 + c2 + c3)

          ef_total := ef_direto + ef_indireto
         '

sem_model <- sem(model, data = am, estimator ="MLR",
                 missing = "ml", mimic ="Mplus")

summary(sem_model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

### Modelo Quase completamente não paramétrico de Mediação Causal (Wood)
non_pa_dem  <- gam(satisfdem ~ s(conf, k = 4) + s(segur, k = 4) + s(punitiv, k = 7) + vitima,
                   fit = TRUE, data = am, method = 'REML')

summary(non_pa_dem)

non_pa_tru  <- gam(conf ~ s(segur, k = 4) + s(punitiv, k = 7) + vitima,
                   fit = TRUE, data = am, method = 'REML')

summary(non_pa_tru)

## Calculando F da Vitimização
f_tes <- gam(satisfdem ~ s(conf, k = 4) + s(segur, k = 4) + s(punitiv, k = 7),
             fit = TRUE, data = am, method = 'REML')

vit_F <-  sum(f_tes$residuals^2) - sum(non_pa_dem$residuals^2)
vit_F <- vit_F * nrow(am) - 3 - 1
vit_F <- vit_F / sum(non_pa_dem$residuals^2)
# F Vitimização: Modelo 1
print(vit_F)
#
f_tes <-  gam(conf ~ s(segur, k = 4) + s(punitiv, k = 7),
              fit = TRUE, data = am, method = 'REML')

vit_F <-  sum(f_tes$residuals^2) - sum(non_pa_tru$residuals^2)
vit_F <- vit_F * nrow(am) - 3 - 1
vit_F <- vit_F / sum(non_pa_tru$residuals^2)
# F Vitimização: Modelo 2
print(vit_F)

### Modelo Mediação Causal (Imai)
mediator <- non_pa_tru

general <- non_pa_dem

med_vit <- mediate(mediator, general, treat = 'vitima', mediator = 'conf',
                   robustSE = TRUE, sims = 100, boot = TRUE)

med_pun <- mediate(mediator, general, treat = 'punitiv', mediator = 'conf',
                   robustSE = TRUE, sims = 100, boot = TRUE)

med_seg <- mediate(mediator, general, treat = 'segur', mediator = 'conf',
                   robustSE = TRUE, sims = 100, boot = TRUE)

summary(med_vit)
summary(med_pun)
summary(med_seg)

