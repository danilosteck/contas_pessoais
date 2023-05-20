source('pegar_extratos_bradesco.R', encoding = 'UTF-8')
source('pegar_extratos_nubank.R', encoding = 'UTF-8')
source('pegar_extratos_cartao_flash.R', encoding = 'UTF-8')
source('funcao_categorias.R', encoding = 'UTF-8')

ult_data <- function(df){
  df %>% select(data) %>% arrange(desc(data)) %>% head(1)
}

split_cat <- function(cat){
  desp_comp <- str_split(cat,' \\|\\| ')[[1]][2]
}

extrato_unificado <- bind_rows(extrato_bradesco_final,
                               extrato_nubank_pf,
                               extrato_nubank_pj,
                               extrato_cartao_flash)

extrato_unificado <- extrato_unificado %>% 
  mutate(descricao = paste(desc1, desc2, sep = ' || '))

extrato_cat <- categorizar_gastos(extrato_unificado) %>% 
  mutate(ano = year(data),
         mes = month(data),
         dia = day(data),
         compartilhado = sapply(strsplit((categoria), split=" \\|\\| "), function(x) x[2]),
         valor_texto = str_replace(as.character(-valor_brl),'\\.',',')) %>% 
  arrange(data) %>% 
  unique()

ult_data(extrato_cat)

data_inicio <- floor_date(floor_date(Sys.Date(), unit = "month") - 1, unit = "month")
data_fim <- floor_date(Sys.Date(), unit = "month") - 1

dir_path <- glue('./extratos_consolidados/consolidado/{Sys.Date()}')
if(!dir.exists(dir_path)){dir.create(path = dir_path)}
write.csv(extrato_cat, glue('{dir_path}/extrato_consolidado_gerado_em_{Sys.Date()}.csv'))
write.csv((extrato_cat %>% filter(data >= data_inicio, data <= data_fim)), glue('{dir_path}/extrato_mensal_gerado_em_{Sys.Date()}.csv'))
