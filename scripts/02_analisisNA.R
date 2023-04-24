library(readr)
library(tidyr)
library(dplyr)
library(lubridate)

data_prec <- readRDS("data/data_procesada/data_prec.rds")
metadata <- readRDS("data/data_procesada/metadata.rds")

#visualizacion de data procesada anios y estaciones distinct----
data_prec |> 
  mutate(fecha = as_date(fecha)) |> 
  mutate(anio = year(fecha)) |> 
  distinct(anio) |> 
  glimpse()

data_prec |> 
  mutate(fecha = as_date(fecha))|> 
  distinct(year(fecha)) |> 
  glimpse()

#datos desde 1981 sin NA (-9999)
data_1981_2020_na <- data_prec |> 
  mutate(fecha = as_date(fecha)) |>
  filter(year(fecha) >= 1981) |> 
  mutate(valor = replace(valor, valor == -9999,NA))


data_1981_2020_na_prop <- data_1981_2020_na |> 
  group_by(codigo_estacion) |> 
  summarise(n_na = sum(is.na(valor)), n_valor = n()) |> 
  mutate(prop_na = n_na/n_valor, categ = cut(prop_na, breaks=4))

data_1981_2020_na_prop <-  as.data.frame(data_1981_2020_na_prop)

saveRDS(data_1981_2020_na_prop,file = "data/data_procesada/data_1981_2020_na_prop.rds")

data_1981_2020_na_prop |> 
  group_by(categ) |> 
  summarize(n = n())



