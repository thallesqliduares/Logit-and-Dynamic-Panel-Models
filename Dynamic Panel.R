
# Desafio Econometria II --------------------------------------------------




# Import Libraries --------------------------------------------------------
library(PNADcIBGE)
library(tidyverse)
library(plm)




# Specify Working Directory -----------------------------------------------
setwd("C:\\Users\\jp-08\\Documents\\Doutorado\\Disciplinas\\Semestre II\\Econometria II\\Desafio\\Parte II")


# Import Data Set ---------------------------------------------------------

# Optei por importar offline, por conveniência (e deflacionar direto)
pnadc.0119 <- read_pnadc("PNADC_012019.txt", "input_PNADC_trimestral.txt", 
                         vars = c("Ano", "Trimestre", "UF", "UPA", "Estrato", "V1008", "V1014", "V1016", "V1022", 
                                  "V2003", "V2007", "V2008", "V20081", "V20082", "V2009", "V2010", "VD2002", 
                                  "VD2003", "VD3004", "VD3005", "VD4001", "VD4002", "VD4009", "VD4019", "VD4020")) %>%
  pnadc_deflator( . , "deflator_PNADC_2020_trimestral_040506.xls")

pnadc.0219 <- read_pnadc("PNADC_022019.txt", "input_PNADC_trimestral.txt",
                         vars = c("Ano", "Trimestre", "UF", "UPA", "Estrato", "V1008", "V1014", "V1016", "V1022", 
                                  "V2003", "V2007", "V2008", "V20081", "V20082", "V2009", "V2010", "VD2002", 
                                  "VD2003", "VD3004", "VD3005", "VD4001", "VD4002", "VD4009", "VD4019", "VD4020")) %>%
  pnadc_deflator( . , "deflator_PNADC_2020_trimestral_040506.xls")

pnadc.0319 <- read_pnadc("PNADC_032019.txt", "input_PNADC_trimestral.txt",
                         vars = c("Ano", "Trimestre", "UF", "UPA", "Estrato", "V1008", "V1014", "V1016", "V1022", 
                                  "V2003", "V2007", "V2008", "V20081", "V20082", "V2009", "V2010", "VD2002", 
                                  "VD2003", "VD3004", "VD3005", "VD4001", "VD4002", "VD4009", "VD4019", "VD4020")) %>%
  pnadc_deflator( . , "deflator_PNADC_2020_trimestral_040506.xls")

pnadc.0419 <- read_pnadc("PNADC_042019.txt", "input_PNADC_trimestral.txt",
                         vars = c("Ano", "Trimestre", "UF", "UPA", "Estrato", "V1008", "V1014", "V1016", "V1022", 
                                  "V2003", "V2007", "V2008", "V20081", "V20082", "V2009", "V2010", "VD2002", 
                                  "VD2003", "VD3004", "VD3005", "VD4001", "VD4002", "VD4009", "VD4019", "VD4020")) %>%
  pnadc_deflator( . , "deflator_PNADC_2020_trimestral_040506.xls")

pnadc.0120 <- read_pnadc("PNADC_012020.txt", "input_PNADC_trimestral.txt",
                         vars = c("Ano", "Trimestre", "UF", "UPA", "Estrato", "V1008", "V1014", "V1016", "V1022", 
                                  "V2003", "V2007", "V2008", "V20081", "V20082", "V2009", "V2010", "VD2002", 
                                  "VD2003", "VD3004", "VD3005", "VD4001", "VD4002", "VD4009", "VD4019", "VD4020")) %>%
  pnadc_deflator( . , "deflator_PNADC_2020_trimestral_040506.xls")






###########################################################################
## Define Variables --------------------------------------------------------
###########################################################################

pnadc.01 <- filter(pnadc.0119, 
                   !(is.na(VD3005)), # Excluir observações sem informação de Anos de Estudo
                   (VD2002 <= 15)) %>% # Excluir Empregados Domésticos e Pensionistas
  replace_na(list(VD4001 = "00", 
                  VD4002 = "00", 
                  VD4009 = "00", 
                  VD4019 = "00" )) %>%
  transform( . ,
             TRIMESTRE = as.numeric(Trimestre),
             condition = as.numeric(VD2002),# Quem é na família
             fam.size = as.numeric(VD2003), #quantidade de pessoas no domicílio
             gender.id = ifelse(as.numeric(V2007) == 1, 1, 0), # mulher = 1 
             age.id = as.numeric(V2009),#idade na data da entrevista
             school.id = as.numeric(VD3005), # Anos de Estudo
             pea.id = ifelse(as.numeric(VD4001) == 1, 1, 0), # Pessoas na força de trabalho
             status.id = ifelse(as.numeric(VD4002) == 1, 1, 0), # Pessoas ocupadas
             formal.id = ifelse(as.numeric(VD4009) == 1 |
                                  as.numeric(VD4009) == 3 |
                                  as.numeric(VD4009) == 5 |
                                  as.numeric(VD4009) == 7 |
                                  as.numeric(VD4009) == 8, 1, 0), #formal privado, publico, doméstico com carteira, militar, servidor estatutário e empregador
             income.id = as.numeric(VD4019)*Habitual,  # Rendimento mensal habitual de todos os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
             experience.id = as.numeric(V2009) - as.numeric(VD3005) - 7, # Medida de Experiência = idade - anos de estudo - 7 (entrada na primeira série)
             ID_DOMIC = as.numeric(paste0(UPA, V1008, V1014)), # id do domicílio
             skin.id = as.numeric(V2010), #Cor do entrevistado
             urban.id = as.numeric(V1022)
  ) %>%
  select( . , 
          TRIMESTRE, condition, ID_DOMIC, fam.size, gender.id, age.id, skin.id, school.id,
          pea.id, status.id, formal.id, income.id, experience.id, urban.id) %>%
  group_by(ID_DOMIC) %>%
  mutate(income.household = sum(income.id),#soma da renda de todos os integrantes da família
         age.head = ifelse(condition == 1, 
                           age.id, 0),#idade do responsável pelo domicílio
         school.head = ifelse(condition == 1, 
                              school.id, 0),# escolaridade do responsável 
         exp.head = ifelse(condition == 1,
                           experience.id, 0), # EXPERIENCIA do responsável
         skin.head = ifelse(condition == 1, 
                            skin.id, 0), # Cor do responsável
         urban = mean(urban.id), 
         gender.head = ifelse(condition == 1, 
                              gender.id, 0), # Sexo do responsável
         couple = ifelse(condition == 2 | condition == 3, 
                         1, 0), #se é cônjuge ou filho do responsável
  ) %>% 
  filter( . , 
          age.id >= 15) %>% #apenas domicílios em que o responsável tenha 15 anos ou mais são considerados
  mutate(IDADE = mean(age.id), 
         IDADE_sqr = IDADE**2,
         EXPERIENCIA = mean(experience.id), # Experiência média de todos os moradores do domicílio
         ESCOLARIDADE = mean(school.id), # escolaridade média de todos os moradores do domicílio
         MINORIA = max(skin.head), #Cor do responsável
         SEXO = max(gender.head), #Sexo do responsável
         CASAL = max(couple), #Considera que trata-se de um domicílio com cônjuge ou filho
         RENDA_PER = income.household / fam.size, #renda per capita
         PEA = mean(pea.id), # Percentual de PEA entre os adultos
         OCUPACAO = mean(status.id), # Taxa de Ocupados entre os adultos
         RURAL = ifelse(urban == 2, 1, 0) #rural = 1
  ) %>%  
  filter( . ,
          status.id == 1) %>%
  mutate(TX_FORMALIDADE = mean(formal.id)) %>% # Taxa de Formalidade entre os ocupados
  distinct( . , 
            ID_DOMIC, .keep_all = T) %>%
  select( . , 
          ID_DOMIC, TRIMESTRE, RENDA_PER, IDADE, IDADE_sqr, EXPERIENCIA, ESCOLARIDADE, PEA, OCUPACAO, TX_FORMALIDADE, MINORIA,SEXO, CASAL, RURAL)






pnadc.02 <- filter(pnadc.0219, 
                   !(is.na(VD3005)), # Excluir observações sem informação de Anos de Estudo
                   (VD2002 <= 15)) %>% # Excluir Empregados Domésticos e Pensionistas
  replace_na(list(VD4001 = "00", 
                  VD4002 = "00", 
                  VD4009 = "00", 
                  VD4019 = "00" )) %>%
  transform( . ,
             TRIMESTRE = as.numeric(Trimestre),
             condition = as.numeric(VD2002),
             fam.size = as.numeric(VD2003), 
             gender.id = ifelse(as.numeric(V2007) == 1, 1, 0), 
             age.id = as.numeric(V2009),
             school.id = as.numeric(VD3005), # Anos de Estudo
             pea.id = ifelse(as.numeric(VD4001) == 1, 1, 0), # Pessoas na força de trabalho
             status.id = ifelse(as.numeric(VD4002) == 1, 1, 0), # Pessoas ocupadas
             formal.id = ifelse(as.numeric(VD4009) == 1 |
                                  as.numeric(VD4009) == 3 |
                                  as.numeric(VD4009) == 5 |
                                  as.numeric(VD4009) == 7 |
                                  as.numeric(VD4009) == 8, 1, 0), 
             income.id = as.numeric(VD4019)*Habitual, 
             experience.id = as.numeric(V2009) - as.numeric(VD3005) - 7, # Medida de Experiência
             ID_DOMIC = as.numeric(paste0(UPA, V1008, V1014)), 
             skin.id = as.numeric(V2010), 
             urban.id = as.numeric(V1022)
  ) %>%
  select( . , 
          TRIMESTRE, condition, ID_DOMIC, fam.size, gender.id, age.id, skin.id, school.id,
          pea.id, status.id, formal.id, income.id, experience.id, urban.id) %>%
  group_by(ID_DOMIC) %>%
  mutate(income.household = sum(income.id),
         age.head = ifelse(condition == 1, 
                           age.id, 0),
         school.head = ifelse(condition == 1, 
                              school.id, 0),
         exp.head = ifelse(condition == 1,
                           experience.id, 0), 
         skin.head = ifelse(condition == 1, 
                            skin.id, 0), 
         urban = mean(urban.id), 
         gender.head = ifelse(condition == 1, 
                              gender.id, 0), 
         couple = ifelse(condition == 2 | condition == 3, 
                         1, 0), 
  ) %>% 
  filter( . , 
          age.id >= 15) %>%
  mutate(IDADE = mean(age.id),
         IDADE_sqr = IDADE**2,
         EXPERIENCIA = mean(experience.id), 
         ESCOLARIDADE = mean(school.id),
         MINORIA = max(skin.head),
         SEXO = max(gender.head),
         CASAL = max(couple),
         RENDA_PER = income.household / fam.size,
         PEA = mean(pea.id), # Percentual de PEA entre os adultos
         OCUPACAO = mean(status.id), # Taxa de Ocupados entre os adultos
         RURAL = ifelse(urban == 2, 1, 0)
  ) %>%  
  filter( . ,
          status.id == 1) %>%
  mutate(TX_FORMALIDADE = mean(formal.id)) %>% # Taxa de Formalidade entre os ocupados
  distinct( . , 
            ID_DOMIC, .keep_all = T) %>%
  select( . , 
          ID_DOMIC, TRIMESTRE, RENDA_PER, IDADE, IDADE_sqr, EXPERIENCIA, ESCOLARIDADE, PEA, OCUPACAO, TX_FORMALIDADE, MINORIA,SEXO, CASAL, RURAL)





pnadc.03 <- filter(pnadc.0319, 
                   !(is.na(VD3005)), # Excluir observações sem informação de Anos de Estudo
                   (VD2002 <= 15)) %>% # Excluir Empregados Domésticos e Pensionistas
  replace_na(list(VD4001 = "00", 
                  VD4002 = "00", 
                  VD4009 = "00", 
                  VD4019 = "00" )) %>%
  transform( . ,
             TRIMESTRE = as.numeric(Trimestre),
             condition = as.numeric(VD2002),
             fam.size = as.numeric(VD2003), 
             gender.id = ifelse(as.numeric(V2007) == 1, 1, 0), 
             age.id = as.numeric(V2009),
             school.id = as.numeric(VD3005), # Anos de Estudo
             pea.id = ifelse(as.numeric(VD4001) == 1, 1, 0), # Pessoas na força de trabalho
             status.id = ifelse(as.numeric(VD4002) == 1, 1, 0), # Pessoas ocupadas
             formal.id = ifelse(as.numeric(VD4009) == 1 |
                                  as.numeric(VD4009) == 3 |
                                  as.numeric(VD4009) == 5 |
                                  as.numeric(VD4009) == 7 |
                                  as.numeric(VD4009) == 8, 1, 0), 
             income.id = as.numeric(VD4019)*Habitual, 
             experience.id = as.numeric(V2009) - as.numeric(VD3005) - 7, # Medida de Experiência
             ID_DOMIC = as.numeric(paste0(UPA, V1008, V1014)), 
             skin.id = as.numeric(V2010), 
             urban.id = as.numeric(V1022)
  ) %>%
  select( . , 
          TRIMESTRE, condition, ID_DOMIC, fam.size, gender.id, age.id, skin.id, school.id,
          pea.id, status.id, formal.id, income.id, experience.id, urban.id) %>%
  group_by(ID_DOMIC) %>%
  mutate(income.household = sum(income.id),
         age.head = ifelse(condition == 1, 
                           age.id, 0),
         school.head = ifelse(condition == 1, 
                              school.id, 0),
         exp.head = ifelse(condition == 1,
                           experience.id, 0), 
         skin.head = ifelse(condition == 1, 
                            skin.id, 0), 
         urban = mean(urban.id), 
         gender.head = ifelse(condition == 1, 
                              gender.id, 0), 
         couple = ifelse(condition == 2 | condition == 3, 
                         1, 0), 
  ) %>% 
  filter( . , 
          age.id >= 15) %>%
  mutate(IDADE = mean(age.id),
         IDADE_sqr = IDADE**2,
         EXPERIENCIA = mean(experience.id), 
         ESCOLARIDADE = mean(school.id),
         MINORIA = max(skin.head),
         SEXO = max(gender.head),
         CASAL = max(couple),
         RENDA_PER = income.household / fam.size,
         PEA = mean(pea.id), # Percentual de PEA entre os adultos
         OCUPACAO = mean(status.id), # Taxa de Ocupados entre os adultos
         RURAL = ifelse(urban == 2, 1, 0)
  ) %>%  
  filter( . ,
          status.id == 1) %>%
  mutate(TX_FORMALIDADE = mean(formal.id)) %>% # Taxa de Formalidade entre os ocupados
  distinct( . , 
            ID_DOMIC, .keep_all = T) %>%
  select( . , 
          ID_DOMIC, TRIMESTRE, RENDA_PER, IDADE, IDADE_sqr, EXPERIENCIA, ESCOLARIDADE, PEA, OCUPACAO, TX_FORMALIDADE, MINORIA,SEXO, CASAL, RURAL)



pnadc.04 <- filter(pnadc.0419, 
                   !(is.na(VD3005)), # Excluir observações sem informação de Anos de Estudo
                   (VD2002 <= 15)) %>% # Excluir Empregados Domésticos e Pensionistas
  replace_na(list(VD4001 = "00", 
                  VD4002 = "00", 
                  VD4009 = "00", 
                  VD4019 = "00" )) %>%
  transform( . ,
             TRIMESTRE = as.numeric(Trimestre),
             condition = as.numeric(VD2002),
             fam.size = as.numeric(VD2003), 
             gender.id = ifelse(as.numeric(V2007) == 1, 1, 0), 
             age.id = as.numeric(V2009),
             school.id = as.numeric(VD3005), # Anos de Estudo
             pea.id = ifelse(as.numeric(VD4001) == 1, 1, 0), # Pessoas na força de trabalho
             status.id = ifelse(as.numeric(VD4002) == 1, 1, 0), # Pessoas ocupadas
             formal.id = ifelse(as.numeric(VD4009) == 1 |
                                  as.numeric(VD4009) == 3 |
                                  as.numeric(VD4009) == 5 |
                                  as.numeric(VD4009) == 7 |
                                  as.numeric(VD4009) == 8, 1, 0), 
             income.id = as.numeric(VD4019)*Habitual, 
             experience.id = as.numeric(V2009) - as.numeric(VD3005) - 7, # Medida de Experiência
             ID_DOMIC = as.numeric(paste0(UPA, V1008, V1014)), 
             skin.id = as.numeric(V2010), 
             urban.id = as.numeric(V1022)
  ) %>%
  select( . , 
          TRIMESTRE, condition, ID_DOMIC, fam.size, gender.id, age.id, skin.id, school.id,
          pea.id, status.id, formal.id, income.id, experience.id, urban.id) %>%
  group_by(ID_DOMIC) %>%
  mutate(income.household = sum(income.id),
         age.head = ifelse(condition == 1, 
                           age.id, 0),
         school.head = ifelse(condition == 1, 
                              school.id, 0),
         exp.head = ifelse(condition == 1,
                           experience.id, 0), 
         skin.head = ifelse(condition == 1, 
                            skin.id, 0), 
         urban = mean(urban.id), 
         gender.head = ifelse(condition == 1, 
                              gender.id, 0), 
         couple = ifelse(condition == 2 | condition == 3, 
                         1, 0), 
  ) %>% 
  filter( . , 
          age.id >= 15) %>%
  mutate(IDADE = mean(age.id),
         IDADE_sqr = IDADE**2,
         EXPERIENCIA = mean(experience.id), 
         ESCOLARIDADE = mean(school.id),
         MINORIA = max(skin.head),
         SEXO = max(gender.head),
         CASAL = max(couple),
         RENDA_PER = income.household / fam.size,
         PEA = mean(pea.id), # Percentual de PEA entre os adultos
         OCUPACAO = mean(status.id), # Taxa de Ocupados entre os adultos
         RURAL = ifelse(urban == 2, 1, 0)
  ) %>%  
  filter( . ,
          status.id == 1) %>%
  mutate(TX_FORMALIDADE = mean(formal.id)) %>% # Taxa de Formalidade entre os ocupados
  distinct( . , 
            ID_DOMIC, .keep_all = T) %>%
  select( . , 
          ID_DOMIC, TRIMESTRE, RENDA_PER, IDADE, IDADE_sqr, EXPERIENCIA, ESCOLARIDADE, PEA, OCUPACAO, TX_FORMALIDADE, MINORIA,SEXO, CASAL, RURAL)





pnadc.05 <- filter(pnadc.0120, 
                   !(is.na(VD3005)), # Excluir observações sem informação de Anos de Estudo
                   (VD2002 <= 15)) %>% # Excluir Empregados Domésticos e Pensionistas
  replace_na(list(VD4001 = "00", 
                  VD4002 = "00", 
                  VD4009 = "00", 
                  VD4019 = "00" )) %>%
  transform( . ,
             TRIMESTRE = 5,
             condition = as.numeric(VD2002),
             fam.size = as.numeric(VD2003), 
             gender.id = ifelse(as.numeric(V2007) == 1, 1, 0), 
             age.id = as.numeric(V2009),
             school.id = as.numeric(VD3005), # Anos de Estudo
             pea.id = ifelse(as.numeric(VD4001) == 1, 1, 0), # Pessoas na força de trabalho
             status.id = ifelse(as.numeric(VD4002) == 1, 1, 0), # Pessoas ocupadas
             formal.id = ifelse(as.numeric(VD4009) == 1 |
                                  as.numeric(VD4009) == 3 |
                                  as.numeric(VD4009) == 5 |
                                  as.numeric(VD4009) == 7 |
                                  as.numeric(VD4009) == 8, 1, 0), 
             income.id = as.numeric(VD4019)*Habitual, 
             experience.id = as.numeric(V2009) - as.numeric(VD3005) - 7, # Medida de Experiência
             ID_DOMIC = as.numeric(paste0(UPA, V1008, V1014)), 
             skin.id = as.numeric(V2010), 
             urban.id = as.numeric(V1022)
  ) %>%
  select( . , 
          TRIMESTRE, condition, ID_DOMIC, fam.size, gender.id, age.id, skin.id, school.id,
          pea.id, status.id, formal.id, income.id, experience.id, urban.id) %>%
  group_by(ID_DOMIC) %>%
  mutate(income.household = sum(income.id),
         age.head = ifelse(condition == 1, 
                           age.id, 0),
         school.head = ifelse(condition == 1, 
                              school.id, 0),
         exp.head = ifelse(condition == 1,
                           experience.id, 0), 
         skin.head = ifelse(condition == 1, 
                            skin.id, 0), 
         urban = mean(urban.id), 
         gender.head = ifelse(condition == 1, 
                              gender.id, 0), 
         couple = ifelse(condition == 2 | condition == 3, 
                         1, 0), 
  ) %>% 
  filter( . , 
          age.id >= 15) %>%
  mutate(IDADE = mean(age.id),
         IDADE_sqr = IDADE**2,
         EXPERIENCIA = mean(experience.id), 
         ESCOLARIDADE = mean(school.id),
         MINORIA = max(skin.head),
         SEXO = max(gender.head),
         CASAL = max(couple),
         RENDA_PER = income.household / fam.size,
         PEA = mean(pea.id), # Percentual de PEA entre os adultos
         OCUPACAO = mean(status.id), # Taxa de Ocupados entre os adultos
         RURAL = ifelse(urban == 2, 1, 0)
  ) %>%  
  filter( . ,
          status.id == 1) %>%
  mutate(TX_FORMALIDADE = mean(formal.id)) %>% # Taxa de Formalidade entre os ocupados
  distinct( . , 
            ID_DOMIC, .keep_all = T) %>%
  select( . , 
          ID_DOMIC, TRIMESTRE, RENDA_PER, IDADE, IDADE_sqr, EXPERIENCIA, ESCOLARIDADE, PEA, OCUPACAO, TX_FORMALIDADE, MINORIA,SEXO, CASAL, RURAL)


# Save Space
rm(pnadc.0119, pnadc.0219, pnadc.0319, pnadc.0419, pnadc.0120)


###########################################################################
# Agregando Dados em Painel -----------------------------------------------
###########################################################################

panel <- bind_rows(pnadc.01, pnadc.02, pnadc.03, pnadc.04, pnadc.05) %>%
  arrange( . , 
           ID_DOMIC) %>%
  select( . , 
          ID_DOMIC, TRIMESTRE, RENDA_PER, IDADE, IDADE_sqr, ESCOLARIDADE, MINORIA, PEA, SEXO, EXPERIENCIA, CASAL, OCUPACAO, TX_FORMALIDADE, RURAL) %>%
  filter( . , 
          !(IDADE == 0)) %>%
  group_by(ID_DOMIC) %>%
  filter( . , 
          !(RENDA_PER <= 1), 
          !(ESCOLARIDADE == 0)) %>%
  filter( . , 
          n() == 5 ) %>%
  mutate(RENDA_PER = log(RENDA_PER),
         MINORIA = ifelse(MINORIA == 2| MINORIA == 4| MINORIA == 5, 1, 0),
         SEXO = ifelse(SEXO == 2, 1, 0))


pdim(panel)

panell <- data.frame(panel)
panel1 <- pdata.frame(panell, index = c("ID_DOMIC", "TRIMESTRE"))


rm(pnadc.01, pnadc.02, pnadc.03, pnadc.04, pnadc.05)








###########################################################################
# Dynamic Panel Estimation ------------------------------------------------
###########################################################################

#ID_DOMIC, TRIMESTRE, RENDA_PER, IDADE, IDADE_sqr, ESCOLARIDADE, MINORIA, PEA, SEXO, EXPERIENCIA, CASAL, OCUPACAO, TX_FORMALIDADE, RURAL

# Time Effects Within Model -----------------------------------------------

ols <- plm(RENDA_PER ~ lag(RENDA_PER) + IDADE + IDADE_sqr + MINORIA + SEXO + RURAL + EXPERIENCIA + ESCOLARIDADE + TX_FORMALIDADE + TRIMESTRE - 1, 
           data = panel1, index = c("ID_DOMIC", "TRIMESTRE"), 
           model = "pooling")

summary(ols)

library(stargazer)

stargazer(ols, type = "html", out = "ols.html")

stargazer(ols)


ols2 <- plm(RENDA_PER ~ lag(RENDA_PER) + IDADE + IDADE_sqr +  MINORIA + SEXO + RURAL + EXPERIENCIA + ESCOLARIDADE + TX_FORMALIDADE,
            data = panel1, index = c("ID_DOMIC", "TRIMESTRE"),
            model = "within", effect = "time")
coef(summary(ols2))
summary(ols2)

stargazer(ols2, type = "html", out = "ols2.html")

stargazer(ols2)


# Two-Ways Within Model ---------------------------------------------------

Within <- update(ols2, effect = "twoways")
summary(Within)
stargazer(Within, type = "html", out = "Within.html")

stargazer(Within)

## Estes dois modelos acima tem estimativas inconsistentes. 
## Croissant & Millo (2019) 












###########################################################################
###########################################################################

# Anderson and Hsiao Estimator --------------------------------------------

hsiao <- plm(diff(RENDA_PER) ~ lag(diff(RENDA_PER)) + IDADE + IDADE_sqr + EXPERIENCIA + ESCOLARIDADE + TRIMESTRE + TX_FORMALIDADE - 1 |
               lag(RENDA_PER, 2) + IDADE + IDADE_sqr + EXPERIENCIA + ESCOLARIDADE + TRIMESTRE + TX_FORMALIDADE, 
             data = panel1, index = c("ID_DOMIC", "TRIMESTRE"), 
             model = "pooling")

summary(hsiao)

stargazer(hsiao, type = "html", out = "hsiao.html")

stargazer(hsiao)

hsiao2 <- plm(diff(RENDA_PER) ~ lag(diff(RENDA_PER)) + IDADE + IDADE_sqr +  ESCOLARIDADE + MINORIA + SEXO + RURAL + EXPERIENCIA + TX_FORMALIDADE + TRIMESTRE - 1 |
                lag(RENDA_PER, 2) + IDADE + IDADE_sqr + ESCOLARIDADE + MINORIA + SEXO + RURAL + EXPERIENCIA + TRIMESTRE  + TX_FORMALIDADE, 
              data = panel1, index = c("ID_DOMIC", "TRIMESTRE"), 
              model = "pooling")

summary(hsiao2)
stargazer(hsiao2, type = "html", out = "hsiao2.html")

stargazer(hsiao2)

#IDADE, ESCOLARIDADE, MINORIA, SEXO, CASAL, OCUPACAO, TX_FORMALIDADE, RURAL




###########################################################################
###########################################################################


# Difference GMM Estimator ------------------------------------------------

diff1 <- pgmm(RENDA_PER ~ lag(RENDA_PER) + IDADE + IDADE_sqr + ESCOLARIDADE + TX_FORMALIDADE |
                lag(RENDA_PER, 2) | lag(IDADE) + lag(IDADE_sqr) + lag(ESCOLARIDADE) + lag(TX_FORMALIDADE), 
              data = panel1, index = c("ID_DOMIC", "TRIMESTRE"), 
              model = "twosteps", effect = "twoways", 
              collapse = F, robust = T)

summary(diff1)
stargazer(diff1, type = "html", out = "diff1.html")

stargazer(diff1)


sargan(diff1)
mtest(diff1, order = 2)




diff2 <- pgmm(RENDA_PER ~ lag(RENDA_PER) + IDADE + ESCOLARIDADE + SEXO + MINORIA + OCUPACAO + TX_FORMALIDADE |
                lag(RENDA_PER, 2) | lag(IDADE) + lag(ESCOLARIDADE) + lag(OCUPACAO) + lag(TX_FORMALIDADE) + lag(SEXO) + lag(MINORIA), 
              data = panel1, index = c("ID_DOMIC", "TRIMESTRE"), 
              model = "twosteps", effect = "twoways", 
              collapse = F, robust = T)

summary(diff2)

stargazer(diff2)



## See limiting the number of instruments












###########################################################################
###########################################################################

# System GMM Estimator ----------------------------------------------------

sys1 <- pgmm(RENDA_PER ~ lag(RENDA_PER) + IDADE + IDADE_sqr + ESCOLARIDADE + TX_FORMALIDADE |
               lag(RENDA_PER, 2) | lag(IDADE) + lag(IDADE_sqr) + lag(ESCOLARIDADE) + lag(TX_FORMALIDADE), 
             data = panel1, index = c("ID_DOMIC", "TRIMESTRE"), 
             model = "twostep", effect = "twoways", 
             transformation = "ld")

summary(sys1)



stargazer(sys1, type = "html", out = "sys1.html")

stargazer(sys1)










# Robust Estimation of Covariance Matrix ----------------------------------

diag(vcov(diff2))

sqrt(diag(vcovHC(diff2)))



# Sargan-Hansen Test ------------------------------------------------------

sargan(diff2)

sargan(sys1)

sargan(hsiao)


# AutoCORrelation Test ----------------------------------------------------

mtest(diff2, order = 2)

mtest(sys1, order = 1)





xx



###########################################################################
###########################################################################
# -------------------------------------------------------------------------
###########################################################################
###########################################################################



# 
# model02 <- pgmm(RENDA_PER ~ lag(RENDA_PER, 1) + IDADE + ESCOLARIDADE + OCUPACAO |
#                   lag(RENDA_PER, 2:3),
#                 data = panel1, #index = c("ID_DOMIC", "TRIMESTRE"), 
#                 model = "twosteps", effect = "twoways", 
#                 transformation = "d")
# 
# summary(model02, robust = F)
# 
# 
# mtest(model02, order = 1)
# sqrt(diag(vcovHC(model02)))
# 



###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
































###########################################################################
###########################################################################
###########################################################################
###########################################################################






























