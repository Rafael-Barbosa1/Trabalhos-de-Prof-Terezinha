#--- Assunto: Relação entre variáveis
#--- Autor: Rafael Barbosa
#--- 22/06/2019


#--- Limpando o espaço

rm(list = ls())


#--- Setando o diretório

setwd("F:/UFPA/Trabalhos/Terezinha_Qualidade_Agua")


#--- Carregando o pacote

if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}


if(!require(readxl)) {
  install.packages("readxl", dependencies = T);
  require(readxl)
}



read_xlsx(path = "dadosmeris1306.xlsx", sheet = "")
