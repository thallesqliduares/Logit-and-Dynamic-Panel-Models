
# Desafio Econometria II ---------------------------------------------------


# Import Libraries --------------------------------------------------------
library(PNADcIBGE)
library(tidyverse)
library(mlogit)
library(numDeriv)
library(aod)

options(scipen = 999) # To avoid scientific notation


# Specify Working Directory -----------------------------------------------
setwd("C:/Users/udiza/OneDrive/�rea de Trabalho/Disciplinas 2020.01/Econometria II/Desafio/PNADC")



# Import Data Set ---------------------------------------------------------

# Optei por importar offline, por conveni�ncia
pnadc.0120 <- read_pnadc("PNADC_012020.txt", "input_PNADC_trimestral.txt", 
                         vars = c("UF", "UPA", "V1008", "V1016", "V1022", "V2003", "V2007", "V2008", 
                                  "V20081", "V20082", "V2009", "V2010", "VD2002", "VD3004", "VD3005", 
                                  "VD4001", "VD4002", "VD4009"))


pnadc.0220 <- read_pnadc("PNADC_022020.txt", "input_PNADC_trimestral.txt", 
                         vars = c("UF", "UPA", "V1008", "V1016", "V1022", "V2003", "V2007", "V2008", 
                                  "V20081", "V20082", "V2009", "V2010", "VD2002", "VD3004", "VD3005", 
                                  "VD4001", "VD4002", "VD4009"))




###########################################################################
# Constru�ndo Vari�veis do Modelo -----------------------------------------
###########################################################################

pnadc.01 <- filter(pnadc.0120, 
                   V1016 != 5, # Exclui observa��es da Quinta Entrevista
                   VD4001 == 1 # Considerar somente PEA
                   ) %>%
    replace_na(list(VD4009 = "00")) %>%
    transform( . ,
               occupation = as.factor(ifelse(as.numeric(VD4009) == 1 |
                                                 as.numeric(VD4009) == 3, "Privado.F", 
                                             ifelse(as.numeric(VD4009) == 2 |
                                                        as.numeric(VD4009) == 4 |
                                                        as.numeric(VD4009) == 10, "Privado.I", 
                                                    ifelse(as.numeric(VD4009) == 5 |
                                                               as.numeric(VD4009) == 6 |
                                                               as.numeric(VD4009) == 7, "Publico", 
                                                           ifelse(as.numeric(VD4009) == 8, "Empregador",
                                                                  ifelse(as.numeric(VD4009) == 9, "Autonomo", "Desempregado" )))))),
               gender = ifelse(as.numeric(V2007) == 1, 1, 0), 
               urban = ifelse(as.numeric(V1022) == 1, 1, 0), 
               age = as.numeric(V2009),
               age.sq = as.numeric(V2009)^2,
               skin = as.factor(ifelse(as.numeric(V2010) == 1, "Branco",
                                       ifelse(as.numeric(V2010) == 2, "Negro",  
                                              ifelse(as.numeric(V2010) == 4, "Pardo", "Outro" )))),
               school = as.numeric(VD3005), 
               region = as.factor(ifelse(as.numeric(UF) == 11 |
                                             as.numeric(UF) == 12 |
                                             as.numeric(UF) == 13 |
                                             as.numeric(UF) == 14 |
                                             as.numeric(UF) == 15 |
                                             as.numeric(UF) == 16 |
                                             as.numeric(UF) == 17, "Norte", 
                                         ifelse(as.numeric(UF) == 21 |
                                                    as.numeric(UF) == 22 |
                                                    as.numeric(UF) == 23 |
                                                    as.numeric(UF) == 24 |
                                                    as.numeric(UF) == 25 |
                                                    as.numeric(UF) == 26 |
                                                    as.numeric(UF) == 27 |
                                                    as.numeric(UF) == 28 |
                                                    as.numeric(UF) == 29, "Nordeste",  
                                                ifelse(as.numeric(UF) == 50 |
                                                           as.numeric(UF) == 51 |
                                                           as.numeric(UF) == 52 |
                                                           as.numeric(UF) == 53, "CentroOeste",  
                                                       ifelse(as.numeric(UF) == 31 |
                                                                  as.numeric(UF) == 32 |
                                                                  as.numeric(UF) == 33 |
                                                                  as.numeric(UF) == 35, "Sudeste", "Sul" ))))), 
               status = ifelse(as.numeric(VD4002) == 1, 1, 0), 
               control = paste0(UPA, V1008, V2003, VD2002, V2007, V2008, V20081, V20082) ) %>%
    select( . , 
            control, status, occupation, gender, urban, age, age.sq, skin, school, region) 



pnadc.02 <- filter(pnadc.0220, 
                   V1016 != 1, # Exclui observa��es da Primeira Entrevista
                   VD4001 == 1 # Considerar somente PEA
                   ) %>%
    replace_na(list(VD4009 = "00")) %>%
    transform( . ,
               occupation = as.factor(ifelse(as.numeric(VD4009) == 1 |
                                                 as.numeric(VD4009) == 3, "Privado.F", 
                                             ifelse(as.numeric(VD4009) == 2 |
                                                        as.numeric(VD4009) == 4 |
                                                        as.numeric(VD4009) == 10, "Privado.I", 
                                                    ifelse(as.numeric(VD4009) == 5 |
                                                               as.numeric(VD4009) == 6 |
                                                               as.numeric(VD4009) == 7, "Publico", 
                                                           ifelse(as.numeric(VD4009) == 8, "Empregador",
                                                                  ifelse(as.numeric(VD4009) == 9, "Autonomo", "Desempregado" )))))),
               gender = ifelse(as.numeric(V2007) == 1, 1, 0), 
               urban = ifelse(as.numeric(V1022) == 1, 1, 0), 
               age = as.numeric(V2009),
               age.sq = as.numeric(V2009)^2,
               skin = as.factor(ifelse(as.numeric(V2010) == 1, "Branco",
                                       ifelse(as.numeric(V2010) == 2, "Negro",  
                                              ifelse(as.numeric(V2010) == 4, "Pardo", "Outro" )))),
               school = as.numeric(VD3005), 
               region = as.factor(ifelse(as.numeric(UF) == 11 |
                                             as.numeric(UF) == 12 |
                                             as.numeric(UF) == 13 |
                                             as.numeric(UF) == 14 |
                                             as.numeric(UF) == 15 |
                                             as.numeric(UF) == 16 |
                                             as.numeric(UF) == 17, "Norte", 
                                         ifelse(as.numeric(UF) == 21 |
                                                    as.numeric(UF) == 22 |
                                                    as.numeric(UF) == 23 |
                                                    as.numeric(UF) == 24 |
                                                    as.numeric(UF) == 25 |
                                                    as.numeric(UF) == 26 |
                                                    as.numeric(UF) == 27 |
                                                    as.numeric(UF) == 28 |
                                                    as.numeric(UF) == 29, "Nordeste",  
                                                ifelse(as.numeric(UF) == 50 |
                                                           as.numeric(UF) == 51 |
                                                           as.numeric(UF) == 52 |
                                                           as.numeric(UF) == 53, "CentroOeste",  
                                                       ifelse(as.numeric(UF) == 31 |
                                                                  as.numeric(UF) == 32 |
                                                                  as.numeric(UF) == 33 |
                                                                  as.numeric(UF) == 35, "Sudeste", "Sul" ))))), 
               status = ifelse(as.numeric(VD4002) == 1, 1, 0), 
               control = paste0(UPA, V1008, V2003, VD2002, V2007, V2008, V20081, V20082) ) %>%
    select( . , 
            control, status, occupation, gender, urban, age, age.sq, skin, school, region) 



## SAVE SPACE
rm(pnadc.0120, pnadc.0220)



###########################################################################
# -------------------------------------------------------------------------
###########################################################################

# Taxa de Desemprego

sum(pnadc.01$status == 0) / sum(pnadc.01$status != 0) 
sum(pnadc.02$status == 0) / sum(pnadc.02$status != 0)


with(pnadc.01, table(school, occupation))
with(pnadc.02, table(school, occupation))






###########################################################################
# -------------------------------------------------------------------------
###########################################################################


# Modelo Log�stico Multinomial (Primeiro Trimestre) -----------------------

mdata01 <- mlogit.data(pnadc.01, choice = "occupation", shape = "wide", sep = "-")

model01 <- mlogit(occupation ~ 1 | age + age.sq + gender + skin + school + region, 
                  data = mdata01, reflevel = "Desempregado")
summary(model01)

# Computar a Raz�o de Risco Relativo
round(exp(coef(model01)), 4)


## Returns the predicted probabilities for the outcome 
head(fitted(model01))
## Returns the predicted probabilities for all the alternatives
head(fitted(model01, outcome = F)) 



## Marginal Effects Analysis

# Set coefficients of Age and School
c.names1 <- names(model01$model)[c(2,6)]

# Get Average Marginal Effects (AME)
AME.model01 <- sapply(c.names1, function(x) 
    effects(model01, covariate = x, data = mdata01), simplify = F)
round((t(sapply(AME.model01, colMeans))), 4)











# Modelo Log�stico Multinomial (Segundo Trimestre) ------------------------

mdata02 <- mlogit.data(pnadc.02, choice = "occupation", shape = "wide", sep = "-")

model02 <- mlogit(occupation ~ 1 |  age + age.sq + gender + skin + school + region,
                  data = mdata02, reflevel = "Desempregado")
summary(model02)

# Computar a Raz�o de Risco Relativo
round(exp(coef(model02)), 4)



## Returns the predicted probabilities for the outcome
head(fitted(model02))
## Returns the predicted probabilities for all the alternatives
head(fitted(model02, outcome = F))


## Marginal Effects Analysis

# Set coefficients of Age and School
c.names2 <- names(model02$model)[c(2,6)]

# Get Average Marginal Effects (AME)
AME.model02 <- sapply(c.names2, function(x) 
    effects(model02, covariate = x, data = mdata02), simplify = F)
round((t(sapply(AME.model02, colMeans))), 4)







###########################################################################
# -------------------------------------------------------------------------
###########################################################################


# Agregando as PNAD Cont�nuas - Por Indiv�duo -----------------------------

Selection <- full_join(pnadc.01, pnadc.02, 
                       by = c("control"), 
                       suffix = c(".01", ".02")) %>%
    filter( . , 
            occupation.01 != "Desempregado", 
            !is.na(occupation.02), 
            !(school.01 != school.02), 
            !(skin.01 != skin.02)) %>%
    select( . ,
            control, occupation.01, ends_with(".02")) %>%
    rename( . , 
            occupation = occupation.01, gender = gender.02, 
            urban = urban.02, age = age.02, age.sq = age.sq.02,
            skin = skin.02, school = school.02, 
            region = region.02, status = status.02)





# # Modelo Log�stico Multinomial - Segundo Trimestre (Condicional) ----------
# 
# mdata03 <- mlogit.data(Selection, choice = "occupation", shape = "wide", sep = "-")
# 
# model03 <- mlogit(occupation ~ 1 | age + age.sq + gender + school + region, 
#                   data = mdata03, reflevel = "Desempregado")
# summary(model02)
# 
# # Computar a Raz�o de Risco Relativo
# round(exp(coef(model02)), 4)
# 
# 
# 
# ## Returns the predicted probabilities for the outcome 
# head(fitted(model02))
# ## Returns the predicted probabilities for all the alternatives
# head(fitted(model02, outcome = F))

















###########################################################################
# -------------------------------------------------------------------------
###########################################################################

# In this section, we estimate a conventional logit model for each occupational group, 
# conditioned to be occupied at the first quarter. 

# That is, occupational group are formed based on the position in the first quarter, 
# and the dependent variable are referent to the second quarter.



# Setor Privado Formal ----------------------------------------------------
PRIF <- filter(Selection, 
               occupation == "Privado.F")

PRIF.logit <- glm(status ~ age + age.sq + gender + skin + school + region,  
               data = PRIF, family = binomial(link = logit))

summary(PRIF.logit)
round(coef(PRIF.logit), digits = 4)
round(exp(coef(PRIF.logit)), 4)




# Setor Privado Formal ----------------------------------------------------
PRII <- filter(Selection, 
               occupation == "Privado.I")

PRII.logit <- glm(status ~ age + age.sq + gender + skin + school + region,  
                  data = PRII, family = binomial(link = logit))

summary(PRII.logit)
round(coef(PRII.logit), digits = 4)
round(exp(coef(PRII.logit)), 4)



# Setor Publico -----------------------------------------------------------
PUBL <- filter(Selection, 
               occupation == "Publico")

PUBL.logit <- glm(status ~ age + age.sq + gender + skin + school + region,  
                  data = PUBL, family = binomial(link = logit))

summary(PUBL.logit)
round(coef(PUBL.logit), digits = 4)
round(exp(coef(PUBL.logit)), 4)





# Empregador --------------------------------------------------------------
EMPR <- filter(Selection, 
               occupation == "Empregador")

EMPR.logit <- glm(status ~ age + age.sq + gender + skin + school + region,  
                  data = EMPR, family = binomial(link = logit))

summary(EMPR.logit)
round(coef(EMPR.logit), digits = 4)
round(exp(coef(EMPR.logit)), 4)





# Autonomo ----------------------------------------------------------------
AUTO <- filter(Selection, 
               occupation == "Autonomo")

AUTO.logit <- glm(status ~ age + age.sq + gender + skin + school + region,  
                  data = AUTO, family = binomial(link = logit))

summary(AUTO.logit)
round(coef(AUTO.logit), digits = 4)
round(exp(coef(AUTO.logit)), 4)





