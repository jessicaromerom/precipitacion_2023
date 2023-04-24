library(readr)
library(tidyr)

data <- read_rds('data/data_procesada/data_prec.rds')


data |> 
  pivot_wider(names_from = 'codigo_estacion',
              values_from = 'valor')
