# Capital-Asset-Pricing-Model---CAPM


##### *LOADING NECESSARY LIBRARY*
```{r, eval=T, echo=F, message=FALSE, warning=F}
library(tidyquant)
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)
```
### *SELECTING IBOVESPA ASSETS FOR THE MODEL*
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
```{r, eval=T, echo=F, message=FALSE, warning=F}
acoes_v <- c("MGLU3.SA", "WEGE3.SA", "POSI3.SA", "WIZS3.SA","PETR4.SA","ITUB4.SA","BBDC4.SA",
             "ITSA4.SA","CSNA3.SA","GGBR4.SA")
```
##### SELECTING ACTIVE COST VALUES (OPENING, HIGH, LOW, CLOSE, VOLUME), FOR A DEFINED PERIOD
```{r, eval=T, echo=F, message=FALSE, warning=F}
acoes_df <- tq_get(acoes_v,
                   from = "2021-01-01", 
                   to = "2022-05-31", 
                   get = "stock.prices") %>% 
  group_by(symbol)
```
##### LOWERING THE BASES OF EXPECTED RETURN(Ri) AND MARKET RETURN(Rm)
```{r, eval=T, echo=F, message=FALSE, warning=F}
#### SELECTING THE EXPECTED RETURN ON THE ASSET IN THE SAME PERIOD
Ri <- acoes_df %>% tq_transmute(select = adjusted,
                                mutate_fun = periodReturn,
                                period = "daily",
                                col_rename = "Ri")


Rm <- "^BVSP" %>%
  tq_get(get  = "stock.prices",
         from = "2021-01-01",
         to = "2022-05-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rm")
```

##### REMOVING MISSING VALUES Ri AND Rm AND JOINING THE BASES
```{r, eval=T, echo=F, message=FALSE, warning=F}
Ri <- na.omit(Ri)
Rm <- na.omit(Rm)

RiRm <- left_join(Ri, Rm, by = c("date" = "date"))
```

##### CREATING THE CAPITAL ASSET PRICING MODEL
```{r, eval=T, echo=F, message=FALSE, warning=F}
x <- tq_performance(RiRm, Ri, Rm, Rf = 0.1275/252, performance_fun = table.CAPM)
kable(x, caption = 'Tabela 1 - CAPITAL ASSET PRICING MODEL - CAPM') %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive"),  fixed_thead = T)%>% 
  scroll_box(width = "800px", height = "400px")
```
