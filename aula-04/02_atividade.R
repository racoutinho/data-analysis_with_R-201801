library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.
salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

head(salarios,20)


salarios%>%
  mutate(REMUNERACAO_FINAL= (REMUNERACAO_REAIS + (REMUNERACAO_DOLARES * 3.2421)))%>%
  filter(REMUNERACAO_FINAL>900)%>%
  select(ID_SERVIDOR_PORTAL, REMUNERACAO_REAIS, REMUNERACAO_DOLARES, REMUNERACAO_FINAL,DATA_INGRESSO_ORGAO,DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO,DESCRICAO_CARGO,ORGSUP_LOTACAO,ORGSUP_EXERCICIO)->
  subset_salarios

subset_salarios%>%
  head(20)

### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

atividade1 <- subset_salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  summarise( SERVIDORES = n(),CORRELACAO = cor(x = ( 2018 - year (DATA_INGRESSO_ORGAO)), 
                                               y = (2018 -year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO))))%>%
  ungroup()%>%
  filter(SERVIDORES >=200)%>%
  arrange(SERVIDORES)%>%
  mutate(DIRECAO = (ifelse(CORRELACAO>0,'POSITIVA','NEGATIVA')))%>%
  mutate(CORRELACAO_ABSOLUTA = (ifelse(CORRELACAO>0,CORRELACAO,(CORRELACAO * (-1)))))%>%
  mutate(FORCA = ifelse(CORRELACAO_ABSOLUTA >=0.9, 'MUITO FORTE',
                 ifelse(CORRELACAO_ABSOLUTA >= 0.7 & CORRELACAO_ABSOLUTA < 0.9, 'FORTE',    
                 ifelse(CORRELACAO_ABSOLUTA >= 0.5 & CORRELACAO_ABSOLUTA < 0.7, 'MODERADA',
                 ifelse(CORRELACAO_ABSOLUTA >= 0.3 & CORRELACAO_ABSOLUTA < 0.5, 'FRACA','DESPREZÍVEL')))))%>%
  select(DESCRICAO_CARGO, 
         CORRELACAO, 
         DIRECAO, 
         FORCA, 
         CORRELACAO_ABSOLUTA) 

atividade1 %>%
  select(DESCRICAO_CARGO, 
         CORRELACAO, 
         DIRECAO, 
         FORCA)
  
  
### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

atividade1 %>%
  arrange(CORRELACAO_ABSOLUTA) %>%
  head(10) %>%
  pull(DESCRICAO_CARGO) -> cargos

subset_salarios %>%
    filter(DESCRICAO_CARGO %in% cargos) %>%
    count(ORGSUP_LOTACAO) %>%
    arrange(desc(n)) %>%
    head(1)%>% 
    pull(ORGSUP_LOTACAO) -> moda_orgsup_lotacaof

subset_salarios %>%
    count(ORGSUP_EXERCICIO) %>%
    arrange(desc(n)) %>%
    head(1)%>% 
    pull(ORGSUP_EXERCICIO) -> moda_orgsup_exerciciof

atividade1%>%
  arrange(CORRELACAO_ABSOLUTA)%>%
  tail(10)%>%
  pull(DESCRICAO_CARGO) -> cargos

subset_salarios %>%
  filter(DESCRICAO_CARGO %in% cargos) %>%
  count(ORGSUP_LOTACAO) %>%
  arrange(desc(n)) %>%
  head(1)%>% 
  pull(ORGSUP_LOTACAO) -> moda_orgsup_lotacao

subset_salarios %>%
  count(ORGSUP_EXERCICIO) %>%
  arrange(desc(n)) %>%
  head(1)%>% 
  pull(ORGSUP_EXERCICIO) -> moda_orgsup_exercicio

print(paste('Dos 10 cargos correlacao mais forte :', 
            paste('Orgao Lotacao:',moda_orgsup_lotacao,'   Orgao Exercicio:',moda_orgsup_exercicio)))

print(paste('Dos 10 cargos correlacao mais fracas : ',
      paste('Orgao Lotacao:',moda_orgsup_lotacaof,'   Orgao Exercicio:',moda_orgsup_exerciciof)))

moda_orgsup_exercicio

##Comentario
## a diferenca das modas  entre orgao de lotacao e de exercio e o exato oposto entre os cargos de 
## correlação mais forte e mais fraca
