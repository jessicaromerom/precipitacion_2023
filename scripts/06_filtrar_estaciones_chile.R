library(readr)
library(dplyr)
library(tidyr)

data <- read_delim('data/data_raw/cr2_prDaily_2020/cr2_prDaily_2020.txt')[-(1:14),]
meta <- read_delim('data/data_raw/cr2_prDaily_2020/cr2_prDaily_2020.txt')[1:14,]

#filtrar datos desde 1981 en adelante
data_pre <- data |> 
  rename(dates = codigo_estacion) |> 
  pivot_longer(-dates,names_to= 'cod_sta') |> 
  mutate(value = as.numeric(value),
         value = case_when(value == -9999 ~ NA,
                           .default = value),
         dates = as.Date(dates)) |> 
  filter(dates >= "1981-01-01") 

# chequear Nas
data_pre |> 
  group_by(cod_sta) |> 
  summarize(n=n(),
            nas = sum(is.na(value)),
            prop = nas/n) |> 
  filter(prop <= 0.8) |> 
  pull(cod_sta) -> sel_cod_sta

#filtrar datos con valores desde cero y que tengan más de 10 años de datos
data_pre |> 
  arrange(cod_sta) |> 
  filter(cod_sta %in% sel_cod_sta & value >= 0) |> 
  group_by(cod_sta) |> 
  summarize(n = n()) |> 
  filter(n > 3650) |> 
  pull(cod_sta) -> sel_cod_sta

data_pre |> 
  arrange(cod_sta) |> 
  filter(cod_sta %in% sel_cod_sta & value >= 0) |> 
  arrange(cod_sta) |> 
  write_rds('data/data_procesada/data_pre_1981_2020_chile.rds')

# guardar metadata de las estaciones
meta |> 
  pivot_longer(-codigo_estacion,names_to='cod_sta') |> 
  pivot_wider(names_from='codigo_estacion',values_from = 'value') |> 
  filter(cod_sta %in% sel_cod_sta) |> 
  arrange(cod_sta) |> 
  write_rds('data/data_procesada/metadata_pre_1981_2020_chile.rds')
         