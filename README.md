# Capital-Asset-Pricing-Model---CAPM

##### *INSTALLING LIBRARY*
install.packages("tidyquant")
install.packages("PerformanceAnalytics")
install.packages("dplyr")
install.packages("tidyverse")

##### *LOADING LIBRARY*
library(tidyquant)
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)

##### *SELECTING IBOVESPA ASSETS FOR THE MODEL*
###### *MGLU3.SA - MAGALU*
###### *WEGE3.SA - WEG ON NM*
###### *POSI3.SA - POSITIVO TECNOLOGY*
###### *WIZS3.SA - WIZ SOLUÇÕES E CORRETAGEM DE SEGUROS SA*
###### *PETR4.SA - PETROBRÁS*
###### *ITUB4.SA - ITAÚ BANK*
###### *BBDC4.SA - BANCO DO BRADESCO*
###### *ITSA4.SA - ITAUSA*
###### *CSNA3.SA - COMPANHIA SIDERÚGICA NACIONAL*
###### *GGBR4.SA - GERDAU*
acoes_v <- c("MGLU3.SA", "WEGE3.SA", "POSI3.SA", "WIZS3.SA","PETR4.SA","ITUB4.SA","BBDC4.SA",
             "ITSA4.SA","CSNA3.SA","GGBR4.SA")

##### SELECTING ACTIVE COST VALUES (OPENING, HIGH, LOW, CLOSE, VOLUME), FOR A DEFINED PERIOD
acoes_df <- tq_get(acoes_v,
                   from = "2021-01-01", 
                   to = "2022-05-31", 
                   get = "stock.prices") %>% 
  group_by(symbol)

view(acoes_df)

tail(acoes_df,10) 
Ri <- acoes_df %>% tq_transmute(select = adjusted,
                                mutate_fun = periodReturn,
                                period = "daily",
                                col_rename = "Ri")

view(Ri)

Rm <- "^BVSP" %>%
  tq_get(get  = "stock.prices",
         from = "2021-01-01",
         to = "2022-05-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rm")
view(Rm)
tail(Rm,10)

#Rm <- Rm[-c(103:112),]

Ri <- na.omit(Ri)
Rm <- na.omit(Rm)

RiRm <- left_join(Ri, Rm, by = c("date" = "date"))

x <- tq_performance(RiRm, Ri, Rm, Rf = 0.1275/252, performance_fun = table.CAPM)
view(x)

# ANÁLISE EXTRA
y = "^BVSP"

BVSP<-tq_get(y,get  = "stock.prices",
         from = "2000-01-01",
         to = "2022-05-31")

BV <- BVSP %>% filter(BVSP$date >= "2021-01-01")
plot(BVSP$date,BVSP$close, type = "l") 
lines(BVSP$date,BVSP$open, type = "l", col = "red")

x$`R-squared`

RiRm %>% group_by(symbol) %>% summarise(Beta = cov(Ri,Rm)/var(Rm)) %>% view()
