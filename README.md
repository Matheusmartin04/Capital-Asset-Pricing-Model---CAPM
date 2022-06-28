## Capital-Asset-Pricing-Model-CAPM
rm(list = ls())

### CARREGANDO As BIBLIOTECAS NECESSÁRIA

library(tidyquant)
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)

### SELEÇÃO DE ATIVOS IBOVESPA PARA O MODELO
*MGLU3.SA - MAGALU*  
*WEGE3.SA - WEG ON NM*  
*POSI3.SA - POSITIVO TECNOLOGY*  
*WIZS3.SA - WIZ SOLUÇÕES E CORRETAGEM DE SEGUROS SA*  
*PETR4.SA - PETROBRÁS*  
*ITUB4.SA - ITAÚ BANK*  
*BBDC4.SA - BANCO DO BRADESCO*  
*ITSA4.SA - ITAUSA*  
*CSNA3.SA - COMPANHIA SIDERÚGICA NACIONAL*  
*GGBR4.SA - GERDAU* 

### CRIANDO O VETOR COM CODIGOS DO ATIVOS
acoes_v <- c("MGLU3.SA", "WEGE3.SA", "POSI3.SA", "WIZS3.SA","PETR4.SA","ITUB4.SA","BBDC4.SA",
             "ITSA4.SA","CSNA3.SA","GGBR4.SA")

### SELEÇÃO DE VALORES DE CUSTO ATIVOS (ABERTURA, ALTO, BAIXO, FECHADO, VOLUME), PARA UM PERÍODO DE 01/jan À 31/05

acoes_df <- tq_get(acoes_v,
                   from = "2021-01-01", 
                   to = "2022-05-31", 
                   get = "stock.prices") %>% 
  group_by(symbol)

### OBTENDO  BASES DE RETORNO ESPERADO(Rf) E RETORNO DE MERCADO(Rm)

### SELECIONANDO O RETORNO ESPERADO DO ATIVO NO MESMO PERÍODO
Ri <- acoes_df %>% tq_transmute(select = adjusted,
                                mutate_fun = periodReturn,
                                period = "daily",
                                col_rename = "Ri")

### SELECIONANDO O RETORNO DE MERCADO DO ATIVO NO MESMO PERÍODO
Rm <- "^BVSP" %>%
  tq_get(get  = "stock.prices",
         from = "2021-01-01",
         to = "2022-05-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rm")


### REMOVENDO VALORES FALTANTES Ri E Rm E UNIÃO DAS BASES

Ri <- na.omit(Ri)
Rm <- na.omit(Rm)

RiRm <- left_join(Ri, Rm, by = c("date" = "date"))


### CRIANDO O MODELO DE PRECIFICAÇÃO DE ATIVOS DE CAPITAL

x <- tq_performance(RiRm, Ri, Rm, Rf = 0.1275/252, performance_fun = table.CAPM)
view(x)

