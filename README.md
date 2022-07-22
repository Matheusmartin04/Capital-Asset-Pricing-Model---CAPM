## Capital-Asset-Pricing-Model-CAPM
rm(list = ls())

### CARREGANDO As BIBLIOTECAS NECESSÁRIA

library(tidyquant)  
library(PerformanceAnalytics)  
library(dplyr)  
library(tidyverse)  

### SELEÇÃO DE ATIVOS ORDINÁRIOS CO MAIS PARTICIPAÇÕES NO IBOVESPA
*PETR3.SA - PETROBRÁS
*B3SA3.SA - Brasil Bolsa Balcao
*VALE3.SA - Vale S.A 
*ABEV3.SA - AMBEV
*BBAS3.SA - BANCO DO BRASIL
*ELET3.SA - ELETROBRÁS
*JBSS3.SA - JBS
*WEGE3.SA - WEG S.A.
*RENT3.SA - Localiza
*SUZB3.SA - Suzano SA
*HAPV3.SA - HAPVIDA
*EQTL3.SA - Equatorial Energia
*LREN3.SA - Lojas Renner
*RDOR3.SA - Rede d or Sao Luiz SA
*BBDC3.SA - BANDO DO BRADESCO
*RADL3.SA - RaiaDrogasil
*CSAN3.SA - Cosan S.A.
*RAIL3.SA - Rumo SA
*VIVT3.SA - Telefônica Brasil
*ENEV3.SA - Eneva

### CRIANDO O VETOR COM CODIGOS DO ATIVOS
acoes_v <- c( "PETR3.SA", "B3SA3.SA", "VALE3.SA","ABEV3.SA", "BBAS3.SA","ELET3.SA","JBSS3.SA","WEGE3.SA","RENT3.SA",
             "SUZB3.SA","HAPV3.SA","EQTL3.SA", "LREN3.SA", "RDOR3.SA","BBDC3.SA","RADL3.SA", "CSAN3.SA",
             "RAIL3.SA","VIVT3.SA", "ENEV3.SA")


### SELEÇÃO DE VALORES DE CUSTO ATIVOS (ABERTURA, ALTO, BAIXO, FECHADO, VOLUME), PARA UM PERÍODO DE 01/jan À 31/05

acoes_df <- tq_get(acoes_v,
                   from = "2022-01-01", 
                   to = "2022-06-30", 
                   get = "stock.prices") %>% 
  group_by(symbol)

### OBTENDO  BASES DE RETORNO ESPERADO(Rf) E RETORNO DE MERCADO(Rm)

### SELECIONANDO O RETORNO ESPERADO DO ATIVO NO MESMO PERÍODO
Ri <- acoes_df %>% tq_transmute(select = adjusted,
                                mutate_fun = periodReturn,
                                period = "daily",
                                col_rename = "Ri")
### CRIANDO TABELA DO RETORNO MÉDIO, DESVIO PADRÃO E COEFICIENTE DE VARIAÇÃO DO ATIVO
W <- Ri %>% group_by(symbol) %>% summarise('média do retorno diários' = round(mean(Ri)*100,4),'Desvio Padrão' = round(sd(Ri),4), 'Coeficiente de variação' = sd(Ri)/abs(mean(Ri))/100) %>% view()

### SELECIONANDO O RETORNO DE MERCADO DO ATIVO NO MESMO PERÍODO

Rm <- "^BVSP" %>%
tq_get(get  = "stock.prices",
         from = "2022-01-03",
         to = "2022-06-30") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rm")


### REMOVENDO VALORES FALTANTES Ri E Rm E UNIÃO DAS BASES

Ri <- na.omit(Ri)
Rm <- na.omit(Rm)

RiRm <- left_join(Ri, Rm, by = c("date" = "date"))


### CRIANDO O MODELO DE PRECIFICAÇÃO DE ATIVOS DE CAPITAL

x <- tq_performance(RiRm, Ri, Rm, Rf = 0.1175/252, performance_fun = table.CAPM)
view(x)

### GRAFICO DO EXCESSO DE RETORNO (ALFHA DE JENSEN)

ggplot(x, aes(y = Alpha, x = symbol))+
  geom_col(col = 'black',fill = 'grey', width = 1)+
  geom_label(aes(label = round(Alpha,4)),position = position_dodge(width = 0.9), size = 3.5)+
  labs(x = "Ação", y="Alfa")+
  scale_x_discrete(labels = c("ABEV3", "B3SA","BBAS3","BBDC3","CSAN3","ELET3","ENEV3","EQTL3",
                              "HAPV3","JBSS3","LREN3","PETR3","RADL3","RAIL3","RDOR3","RENT3","SUZB3", "VALE3","VIVT3","WEGW3"))+
  theme(legend.position = "none")+
  theme_classic()+theme(axis.text = element_text(size=10))

