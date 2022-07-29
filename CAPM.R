rm(list = ls())

library(tidyquant)
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)

# VETOR COM CODIGO DOS ATIVOS SELECIONADOS
acoes_v <- c( "PETR3.SA", "B3SA3.SA", "VALE3.SA","ABEV3.SA", "BBAS3.SA","ELET3.SA","JBSS3.SA","WEGE3.SA","RENT3.SA",
              "SUZB3.SA","HAPV3.SA","EQTL3.SA", "LREN3.SA", "RDOR3.SA","BBDC3.SA","RADL3.SA", "CSAN3.SA",
              "RAIL3.SA","VIVT3.SA", "ENEV3.SA")

# OBTENDO BASE COM PREÇOS DOS ATIVOS
acoes_df <- tq_get(acoes_v,
                   from = "2022-01-01", 
                   to = "2022-06-30", 
                   get = "stock.prices") %>% 
  group_by(symbol)


view(acoes_df)

acoes_df %>% group_by(symbol) %>% summarise('V open' = var(open),'dp open' = sd(open),'v close' = var(close),'dp close' = sd(close)) %>% view()

tail(acoes_df,10) 
# OBTENDO BASE DE RETORNO FINANCEIRO (ATIVOS)
Ri <- acoes_df %>% tq_transmute(select = adjusted,
                                mutate_fun = periodReturn,
                                period = "daily",
                                col_rename = "Ri")

w <- Ri %>% group_by(symbol) %>% summarise('média do retorno diários' = round(mean(Ri)*100,4),'Desvio Padrão' = round(sd(Ri),4), 'Coeficiente de variação' = sd(Ri)/abs(mean(Ri))/100) %>% view()

write.table(w, file="ArquivoDadosExportados.csv", sep="/")


Ri %>% filter(symbol == "ABEV3.SA") %>% summarise('CV' = (sd(Ri)/mean(Ri)))

# OBTENDO BASE DE RETORNO DE MERCADO(IBOVESPA)
Rm <- "^BVSP" %>%
  tq_get(get  = "stock.prices",
         from = "2022-01-03",
         to = "2022-06-30") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rm")


#Rm <- Rm[-c(103:112),]
# REMOVENDO VALORES NA NO DATASET
Ri <- na.omit(Ri)
Rm <- na.omit(Rm)

RiRm <- left_join(Ri, Rm, by = c("date" = "date"))
# MODELAGEM DO CAPM
x <- tq_performance(RiRm, Ri, Rm, Rf = 0.1175/252, performance_fun = table.CAPM)
view(x)
x
write.table(x, file="capm.csv", sep=";")

version
ggplot(x, aes(y = Alpha, x = symbol))+geom_col()

# GRAFICO DE ALPHA DE JENSEN
ggplot(x, aes(y = Alpha, x = symbol))+
  geom_col(col = 'black',fill = 'grey', width = 1)+
  geom_label(aes(label = round(Alpha,4)),position = position_dodge(width = 0.9), size = 3.5)+
  labs(x = "Ação", y="Alfa")+
  scale_x_discrete(labels = c("ABEV3", "B3SA","BBAS3","BBDC3","CSAN3","ELET3","ENEV3","EQTL3",
                              "HAPV3","JBSS3","LREN3","PETR3","RADL3","RAIL3","RDOR3","RENT3","SUZB3", "VALE3","VIVT3","WEGW3"))+
  theme(legend.position = "none")+
  theme_classic()+theme(axis.text = element_text(size=10))
