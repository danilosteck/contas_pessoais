
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)
library(glue)
source('01_funcoes_bradesco.R', encoding = 'UTF-8')


##### Pegar extratos bradesco #####

extratos_path <- '/home/danilo/Documents/MEGAsync/Pessoal/Contabilidade/contas_pessoais/extratos/'
extratos_bradesco <- list.files(path = paste0(extratos_path,'bradesco'))

for(i in 1:length(extratos_bradesco)){
  print(glue('arquivo: {extratos_bradesco[i]} | i = {i}'))
  arquivo_extrato <- extratos_bradesco[i]
  funcao_extrato <- ler_extrato(arquivo_extrato)
  extrato_formatado_t <- funcao_extrato(arquivo_extrato)
  if(i == 1){
    extrato_formatado <- extrato_formatado_t
  }else{
    extrato_formatado <- bind_rows(extrato_formatado, extrato_formatado_t)
  }
}



extrato_bradesco_final <- extrato_formatado
dir_path <- glue('./extratos_consolidados/bradesco/{Sys.Date()}')
if(!dir.exists(dir_path)){dir.create(dir_path)}
write.csv(extrato_formatado, glue('{dir_path}/extrato_bradesco_de_{min(extrato_formatado$data)}_a_{max(extrato_formatado$data[extrato_formatado$tipo == "conta_bradesco"])}_gerado_em_{Sys.Date()}.csv'))












