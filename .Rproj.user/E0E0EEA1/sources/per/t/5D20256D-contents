
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)

#### Funções auxiliares ####


#### Obter extratos PF ####

extratos_path <- '/home/danilo/Documents/MEGAsync/Pessoal/Contabilidade/contas_pessoais/extratos/'
extratos_flash <- list.files(path = paste0(extratos_path,'flash'))

readxl::read_xlsx(paste0(extratos_path,'flash/', extratos_flash[1]))

for(i in 1:length(extratos_flash)){
  arquivo_extrato <- extratos_flash[i]
  extrato <- readxl::read_xlsx(paste0(extratos_path,'flash/',arquivo_extrato))
  
  extrato_ <- extrato %>% 
    mutate(valor_brl = valor,
           codigo = item,
           desc1 = tolower(descricao),
           id = NA,
           desc2 = NA,
           saldo = NA,
           tipo = 'cartao_flash') %>% 
    select(data, codigo, id, desc1, desc2, valor_brl, saldo, tipo)
  
  if(i == 1){
    extrato_formatado <- extrato_
  }else{
    extrato_formatado <- bind_rows(extrato_formatado, extrato_)
  }
}

extrato_cartao_flash <- extrato_formatado %>% unique()

dir_path <- glue('./extratos_consolidados/flash/{Sys.Date()}')
if(!dir.exists(dir_path)){dir.create(path = dir_path)}
write.csv(extrato_formatado, glue('{dir_path}/extrato_cartao_flash_de_{min(extrato_formatado$data)}_a_{max(extrato_formatado$data[extrato_formatado$tipo == "cartao_flash"])}_gerado_em_{Sys.Date()}.csv'))

