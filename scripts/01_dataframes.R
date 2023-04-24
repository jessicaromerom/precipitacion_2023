library(readr)
library(tidyr)
library(dplyr)

data <- read_delim("data/data_raw/cr2_prDaily_2020_ghcn.txt")
#..\Documents\U Mayor\Precipitacion2023\data\data_raw

metadata <- data |> 
  slice(1:14) |> 
  t()

colnames(metadata) <- metadata[1,]
metadata <- metadata[-1,]
metadata <- cbind(codigo_estacion = rownames(metadata), metadata)
rownames(metadata) <- 1:nrow(metadata)


data_prec <- data |> 
  slice(15:nrow(data)) |> 
  t()
colnames(data_prec) <- data_prec[1,]
data_prec <- data_prec[-1,]
data_prec <- cbind(codigo_estacion = rownames(data_prec), data_prec)
rownames(data_prec) <- 1:nrow(data_prec)

data_prec <- as.data.frame(data_prec)
metadata <-  as.data.frame(metadata)

data_prec <- data_prec |> 
  pivot_longer(-c("codigo_estacion"), names_to='fecha', values_to = 'valor') 
  
data_prec <- data_prec |> 
  mutate(valor = replace(valor, valor == -9999,NA))
  

saveRDS(metadata,file = "data/data_procesada/metadata.rds")
saveRDS(data_prec,file = "data/data_procesada/data_prec.rds")
















