load('cleaned.RData')

library(dplyr)
library(tidyr)
library(lubridate)

prec |> 
  mutate(dates = seq(ymd(19810101),ymd(20200331),by=1)) |> 
  pivot_longer(-dates,names_to = 'codigo_estacion',values_to = 'pre') |> 
  write_rds('data/data_procesada/data_1981_2020_chile_qc.rds')
