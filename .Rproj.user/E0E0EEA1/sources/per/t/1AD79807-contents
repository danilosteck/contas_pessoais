source('compilacao_dados.R')
library(ggplot2)
library(plotly)

extrato_cat %>% 
  filter(!(categoria %like% 'investimento')) %>% 
  filter(!(categoria %like% 'Auto')) %>% 
  filter(tipo != 'cartao_brasdesco') %>% 
  mutate(mes = as.yearmon(data),
         tipo_movimentacao = ifelse(valor_brl > 0,'entrada','saida'),
         valor_brl = ifelse(tipo_movimentacao == 'entrada',valor_brl,-valor_brl)) %>% 
  group_by(mes, tipo_movimentacao) %>% 
  summarise(movimentacao = sum(valor_brl)) %>% 
  ggplot(aes(x = mes, y = movimentacao, fill = tipo_movimentacao))+
  geom_bar(stat = 'identity', position = position_dodge())+
  geom_text(aes(label = round(movimentacao,0)), position = position_dodge(width = 0.08), vjust = -0.5)+
  scale_x_yearmon(n=18)

ggplotly(
  extrato_cat %>%
    group_by(data, tipo) %>% 
    filter(tipo != 'cartao_bradesco') %>% 
    summarise(valor_brl = sum(valor_brl, na.rm = T)) %>% 
    ggplot(aes(x = data, y = valor_brl))+
    geom_line(aes(group = tipo, color = tipo))
)

ggplotly(
  extrato_cat %>%
    filter(tipo == 'conta_bradesco') %>% 
    group_by(data) %>% 
    summarise(valor_brl = sum(valor_brl, na.rm = T)) %>% 
    ggplot(aes(x = data, y = cumsum(valor_brl)))+
    geom_line()
)

ggplotly(
  extrato_cat %>%
    filter(tipo != 'cartao_bradesco') %>% 
    mutate(month = as.yearmon(data)) %>% 
    group_by(month, tipo) %>% 
    summarise(valor_brl = sum(valor_brl, na.rm = T)) %>% 
    ggplot(aes(x = data, y = valor_brl, fill = 'tipo'))+
    geom_bar(stat = 'identity', position = position_dodge2())
)

extrato_cat %>%
  filter(tipo == 'cartao_bradesco') %>% 
  group_by(data) %>% 
  as.data.frame()



# extrato_unificado %>% 
#   mutate(descricao = paste(desc1, desc2, sep = ' || ')) %>% 
#   select(descricao) %>% 
#   unique() %>% 
#   arrange(descricao) %>% 
#   write.csv('descricoes.csv')