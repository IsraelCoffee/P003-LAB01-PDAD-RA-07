# 1. Pacotes ----------------------------------------------------------------

# Rode só na primeira vez; depois pode comentar esta linha
install.packages(c("tidyverse", "srvyr", "dplyr", "readr"))

library(tidyverse)
library(srvyr)
library(dplyr)
library(readr)

# 2. Preparação das bases ---------------------------------------------------

unzip("base de dados/Base de moradores.zip", exdir = "base de dados")
unzip("base de dados/Base de domicílios.zip", exdir = "base de dados")

# 3. Importação -------------------------------------------------------------

# Os arquivos usam ";" como separador, por isso read_csv2()
moradores  <- read_csv2("base de dados/moradores.csv")    # uma linha por morador
domicilios <- read_csv2("base de dados/domicilios.csv")   # uma linha por domicílio


# 4. Junção de domicílios e moradores ---------------------------------------

# Chave: A01nficha (número da ficha do domicílio)
# As colunas abaixo existem nas duas bases com o mesmo conteúdo; saem de
# domicilios para não duplicar (.x e .y) na junção
pdad2024 <- moradores %>% 
  dplyr::left_join(domicilios %>% 
                     select(-c(A01uf, localidade, setor_distrito, infla_ipcadf)),
                   by = "A01nficha")

# 5. Plano amostral ---------------------------------------------------------

# Estrato: setor_distrito | Conglomerado: A01nficha (domicílio)

## 5.1 Moradores (unidade de análise: pessoa) ----
# Base pdad2024 e peso_mor
amostra_mor <- 
  survey::svydesign(id = ~A01nficha,
                    strata = ~setor_distrito,
                    weights = ~peso_mor,
                    nest = TRUE,
                    data = pdad2024)

amostra_mor <- srvyr::as_survey(amostra_mor)

## 5.2 Domicílios (unidade de análise: domicílio) ----
# Usa a base domicilios (uma linha por domicílio) e peso_dom.
# Com pdad2024 cada domicílio seria contado uma vez por morador
amostra_dom <- 
  survey::svydesign(id = ~A01nficha,
                    strata = ~setor_distrito,
                    weights = ~peso_dom,
                    nest = TRUE,
                    data = domicilios)

amostra_dom <- srvyr::as_survey(amostra_dom)

## 5.3 Estratos com um único conglomerado ----
# "adjust" evita erro de variância quando um estrato tem uma só unidade
options(survey.lonely.psu = "adjust")