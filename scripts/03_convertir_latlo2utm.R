library(sf)
library(readr)
library(dplyr)

data_m <- read_rds('data/data_procesada/metadata.rds')

data_m |> 
  as_tibble() |> 
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud)) |> 
  st_as_sf(coords = c('longitud','latitud'),crs = 4326) |> 
  st_transform(32719) |> 
  st_coordinates() -> data_m2
  
data_m |> 
  as_tibble() |> 
  cbind(data_m2) |> 
  select(codigo_estacion,altura,X,Y) |> 
  write_rds('data/data_procesada/data_estac_utm.rds')
