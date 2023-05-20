
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)

#### Funções auxiliares ####


#### Obter extratos PF ####

extratos_path <- '/home/danilo/Documents/MEGAsync/Pessoal/Contabilidade/contas_pessoais/extratos/'
extratos_nubank_pf <- list.files(path = paste0(extratos_path,'nubank_pf'))

for(i in 1:length(extratos_nubank_pf)){
  arquivo_extrato <- extratos_nubank_pf[i]
  extrato <- read.csv(paste0(extratos_path,'nubank_pf/',arquivo_extrato))
  
  extrato_ <- extrato %>% 
    mutate(data = as.Date(Data,'%d/%m/%Y'),
           valor_brl = Valor,
           codigo = Identificador,
           desc1 = tolower(Descrição),
           id = NA,
           desc2 = NA,
           saldo = NA,
           tipo = 'conta_nubank_pf') %>% 
    select(data, codigo, id, desc1, desc2, valor_brl, saldo, tipo)
  
  if(i == 1){
    extrato_formatado <- extrato_
  }else{
    extrato_formatado <- bind_rows(extrato_formatado, extrato_) 
  }
}

extrato_nubank_pf <- extrato_formatado %>% unique()

dir_path <- glue('./extratos_consolidados/nubank/{Sys.Date()}')
if(!dir.exists(dir_path)){dir.create(path = dir_path)}
write.csv(extrato_formatado, glue('{dir_path}/extrato_nubank_pf_de_{min(extrato_formatado$data)}_a_{max(extrato_formatado$data[extrato_formatado$tipo == "conta_nubank_pf"])}_gerado_em_{Sys.Date()}.csv'))

#### Obter extratos PJ ####

extratos_path <- '/home/danilo/Documents/MEGAsync/Pessoal/Contabilidade/contas_pessoais/extratos/'
extratos_nubank_pj <- list.files(path = paste0(extratos_path,'nubank_pj'))
i <- 1
for(i in 1:length(extratos_nubank_pj)){
  arquivo_extrato <- extratos_nubank_pj[i]
  extrato <- read.table(paste0(extratos_path,'nubank_pj/',arquivo_extrato),
                        header = F,fill = T,sep = ',',skip = 1)
  
  extrato_ <- extrato %>% 
    mutate(data = as.Date(V1,'%d/%m/%Y'),
           valor_brl = V2,
           codigo = V3,
           desc1 = tolower(V4),
           id = NA,
           desc2 = NA,
           saldo = NA,
           tipo = 'conta_nubank_pj') %>% 
    select(data, codigo, id, desc1, desc2, valor_brl, saldo, tipo)
  
  if(i == 1){
    extrato_formatado <- extrato_
  }else{
    extrato_formatado <- bind_rows(extrato_formatado, extrato_) %>% unique()
  }
}

extrato_nubank_pj <- extrato_formatado %>% unique()


write.csv(extrato_formatado, glue('{dir_path}/extrato_nubank_pj_de_{min(extrato_formatado$data)}_a_{max(extrato_formatado$data[extrato_formatado$tipo == "conta_nubank_pj"])}_gerado_em_{Sys.Date()}.csv'))

