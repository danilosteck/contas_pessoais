library(data.table)
#### Funções auxiliares
extrato_conta_bradesco <- function(extrato){
  
  linhas <- (extrato[,2] %>% grep(pattern = 'Total'))[1]
  
  extrato <- extrato[3:(linhas-1),]
  
  library(zoo)
  extrato <- extrato %>% 
    mutate(
      id = row.names(.),
      id = if_else(V1 == '','',id),
      id = replace(id,id == '',NA),
      id = na.locf(id)
    )
  
  extrato_parte1 <- extrato[extrato$V1 != '',]
  extrato_parte2 <- extrato[extrato$V1 == '',c('V2','id')]
  
  extrato_ <- left_join(extrato_parte1, extrato_parte2, by = 'id')
  extrato_ <- extrato_ %>% 
    rename(
      data = V1,
      id = id,
      codigo = V3,
      desc1 = V2.x,
      desc2 = V2.y,
      credito = V4,
      debito = V5,
      saldo = V6
    ) %>% select(-V7) %>% 
    mutate(
      data = as.Date(data, '%d/%m/%Y'),
      data = as.Date(parse_date_time(data,"ymd")),
      desc1 = tolower(desc1),
      desc2 = tolower(desc2),
      credito = gsub('\\.','',credito) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric(),
      debito = gsub('\\.','',debito) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric(),
      saldo = gsub('\\.','',saldo) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric()
    )
  
  extrato_final <- extrato_ %>% 
    mutate(
      valor_brl = ifelse(is.na(credito),debito,credito),
      tipo = 'conta_bradesco'
    ) %>% 
    select(data, codigo, id, desc1, desc2, valor_brl, saldo, tipo)
  
  return(extrato_final)
}

extrato_cartao_bradesco <- function(extrato){
  considerar_linha <- (!grepl(pattern = '[:space:]',extrato$V1))*grepl(pattern = '[/]',extrato$V1)
  
  extrato_ <- data.frame(data = extrato$V1[considerar_linha == 1],
                         descricao = extrato$V2[considerar_linha == 1],
                         valor_usd = extrato$V3[considerar_linha == 1],
                         valor_brl = extrato$V4[considerar_linha == 1])
  
  if(extrato[3,1] %like% 'Situação do Extrato'){
    ano <- str_sub(extrato[3,1], start = -4)
    mes <- str_sub(extrato[3,1], start = -7, end = -6)
    dia <- str_sub(extrato[3,1], start = -10, end = -9)
    data_fechamento <- as.Date(glue('{ano}-{mes}-{dia}'), '%Y-%m-%d')
  }else{
    ano <- str_sub(extrato[2,1], start = 13, end = 16)
    mes <- as.integer(str_sub(extrato[2,1], start = 10, end = 11))-1
    dia <- '17'
    data_fechamento <- as.Date(glue('{ano}-{mes}-{dia}'), '%Y-%m-%d')
  }
  
  extrato_ <- extrato_ %>% 
    mutate(
      # data = if_else(as.Date(data, '%d/%m')>=Sys.Date(),as.Date(data, '%d/%m') %m+% years(-1),as.Date(data, '%d/%m')),
      # data = as.Date(parse_date_time(data,"ymd")),
      data = data_fechamento,
      desc1 = tolower(descricao),
      valor_usd = -(gsub('\\.','',valor_usd) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric()),
      valor_brl = -(gsub('\\.','',valor_brl) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric())
    )
  
  
  extrato_final <- extrato_ %>% 
    mutate(desc2 = NA,
           codigo = NA,
           id = NA, 
           saldo = NA,
           tipo = 'cartao_brasdesco') %>% 
    select(data, codigo, id, desc1, desc2, valor_brl, saldo, tipo)
  return(extrato_final)
}

extrato_cartao_bradesco_v2 <- function(extrato){
  extrato_ <- data.frame(data = extrato$V1,
                         descricao = extrato$V2,
                         valor_usd = NA,
                         valor_brl = extrato$V3)
  
  extrato_ <- extrato_ %>%
    mutate(data = gsub('AGO','/08', data),
           data = gsub('SET','/09', data),
           data = gsub('OUT','/10', data),
           data = gsub('NOV','/11', data),
           data = gsub('DEZ','/12', data),
           data = gsub('JAN','/01', data),
           data = gsub('FEV','/02', data),
           data = gsub('MAR','/03', data),
           data = gsub('ABR','/04', data),
           data = gsub('MAI','/05', data),
           data = gsub('JUN','/06', data),
           data = gsub('JUL','/07', data)) %>% 
    mutate(
      data = if_else(as.Date(data, '%d/%m')>=Sys.Date(),as.Date(data, '%d/%m') %m+% years(-1),as.Date(data, '%d/%m')),
      data = as.Date(parse_date_time(data,"ymd")),
      desc1 = tolower(descricao),
      valor_usd = -(gsub('\\.','',valor_usd) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric()),
      valor_brl = -(gsub('\\.','',valor_brl) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric())
    )
  
  extrato_final <- extrato_ %>% 
    mutate(desc2 = NA,
           codigo = NA,
           id = NA, 
           saldo = NA,
           tipo = 'cartao_brasdesco') %>% 
    select(data, codigo, id, desc1, desc2, valor_brl, saldo, tipo)
  return(extrato_final)
}

extrato_cartao_bradesco_v3 <- function(extrato){
  extrato_ <- data.frame(data = extrato$V1,
                         descricao = extrato$V2,
                         valor_usd = NA,
                         valor_brl = extrato$V4)
  
  extrato_ <- extrato_ %>%
    mutate(data = gsub('AGO','/08', data),
           data = gsub('SET','/09', data),
           data = gsub('OUT','/10', data),
           data = gsub('NOV','/11', data),
           data = gsub('DEZ','/12', data),
           data = gsub('JAN','/01', data),
           data = gsub('FEV','/02', data),
           data = gsub('MAR','/03', data),
           data = gsub('ABR','/04', data),
           data = gsub('MAI','/05', data),
           data = gsub('JUN','/06', data),
           data = gsub('JUL','/07', data)) %>% 
    mutate(
      data = if_else(as.Date(data, '%d/%m')>=Sys.Date(),as.Date(data, '%d/%m') %m+% years(-1),as.Date(data, '%d/%m')),
      data = as.Date(parse_date_time(data,"ymd")),
      desc1 = tolower(descricao),
      valor_usd = -(gsub('\\.','',valor_usd) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric()),
      valor_brl = -(gsub('\\.','',valor_brl) %>% gsub(pattern = '\\,', replacement = '\\.') %>% as.numeric())
    )
  
  extrato_final <- extrato_ %>% 
    mutate(desc2 = NA,
           codigo = NA,
           id = NA, 
           saldo = NA,
           tipo = 'cartao_brasdesco') %>% 
    select(data, codigo, id, desc1, desc2, valor_brl, saldo, tipo)
  return(extrato_final)
}

ler_extrato <- function(arquivo_extrato){
  info_extrato <- read.table(paste0(extratos_path,'bradesco/',arquivo_extrato), header = F, fill = T, sep = ",",nrows = 1)[1,1]
  sit_extrato <- read.table(paste0(extratos_path,'bradesco/', arquivo_extrato), header = F,fill = T,sep = ';', encoding = 'utf-8')[[1]][2]
  if(info_extrato %like% "Extrato"){
    func <- function(arquivo_extrato){
      extrato_raw <- read.table(paste0(extratos_path,'bradesco/', arquivo_extrato), header = F,fill = T,sep = ';')
      extrato <- extrato_conta_bradesco(extrato_raw)
      return(extrato)
    }
  } else if (info_extrato %like% "Bradesco Internet Banking"){
    func <- function(arquivo_extrato){
      extrato_raw <- read.table(paste0(extratos_path,'bradesco/', arquivo_extrato), header = F,fill = T,sep = ';')
      extrato <- extrato_cartao_bradesco(extrato_raw)
      
      return(extrato)
    }
  } else if(arquivo_extrato %like% 'PM'){
    func <- function(arquivo_extrato){
      first_col <- read.table(paste0(extratos_path,'bradesco/', arquivo_extrato), header = F,fill = T,sep = ';', skip = 6)[[1]]
      
      nrows <- which(first_col== 'Total para DANILO B STECKELBER:')-1
      extrato_raw <- read.table(paste0(extratos_path,'bradesco/', arquivo_extrato), header = F,fill = T,sep = ';', skip = 6,nrows = nrows)
      
      # extrato_raw <- read.table(paste0(extratos_path,'bradesco/', arquivo_extrato), header = F, fill = T, sep = ",", skip = 3)
      extrato <- extrato_cartao_bradesco_v3(extrato_raw)
      return(extrato)
    }
  } else {
    func <- function(arquivo_extrato){
      
      extrato_raw <- read.table(paste0(extratos_path,'bradesco/', arquivo_extrato), header = F, fill = T, sep = ",", skip = 3)
      extrato <- extrato_cartao_bradesco_v2(extrato_raw)
      return(extrato)
    }
  }
  return(func)
}
