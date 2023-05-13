library(readr)
library(sf)

#leer metadata estaciones y obtener coordenadas en UTM
meta <- read_rds('data/data_procesada/metadata_pre_1981_2020_chile.rds') |> 
  select(cod_sta,altura,latitud,longitud) |> 
  st_as_sf(coords = c('longitud','latitud'),crs=4326) |> 
  st_transform(32719) 

# reordenar y renombrar
meta_df <- meta |> 
  st_coordinates() |> 
  bind_cols(meta) |> 
  relocate(cod_sta,.before =X) |>
  relocate(altura,.before= X) |> 
  select(-geometry) |> 
  mutate(altura = as.numeric(altura)) |> 
  rename(ID = cod_sta,ALT = altura) 

#datos precipitación

data_pre <- read_rds('data/data_procesada/data_pre_1981_2020_chile.rds')

data_pre_mat <- data_pre |> 
  pivot_wider(names_from = cod_sta) |> 
  arrange(dates) |> 
  select(-dates) |> 
  as.matrix()

qcPrec(data_pre_mat, meta_df, as.Date("1981-01-01"), as.Date("2020-03-31"), parallel = TRUE, ncpu = 85, printmeta = TRUE, thres = NA)
