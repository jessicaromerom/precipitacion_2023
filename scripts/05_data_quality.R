library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(reddPrec)

data_prec <- read_rds('data/data_procesada/data_prec.rds')
data_estaciones_utm <- read_rds('data/data_procesada/data_estac_utm.rds')
data_1981_2020_na_prop <- read_rds('data/data_procesada/data_1981_2020_na_prop.rds')
data_ubi_utm <- left_join(data_1981_2020_na_prop, data_estaciones_utm, by = 'codigo_estacion') 



#Cluster por cercania----
coordenadas<- data_ubi_utm |> 
  select(X,Y) |> 
  as.matrix()

coordenadas_normalizadas <-  scale(coordenadas)
clusters <- kmeans(coordenadas,center = 4, nstart = 10)

data_cluster <- data_ubi_utm |> 
  mutate(cluster = as.factor(clusters$cluster))

data_cluster <- left_join(data_cluster, select(metadata, codigo_estacion, nombre_cuenca), by = "codigo_estacion")

ggplot(data_cluster, aes(x=X, y=Y, color=cluster)) + 
  geom_point(size=2) + 
  theme_minimal()

ggplot(data_cluster, aes(x=cluster)) +
  geom_bar() +
  theme_minimal()


tabla_frecuencia <- table(data_cluster$categ, data_cluster$cluster)
tabla_frecuencia <- as.data.frame(tabla_frecuencia)
names(tabla_frecuencia) <- c("categ", "cluster", "frecuencia")
#tabla_frecuencia <- aggregate(categ ~ cluster , data=data_cluster, FUN=mean)

ggplot(data = tabla_frecuencia, aes(x = cluster, y = frecuencia, fill = categ)) + 
  geom_bar(stat = "identity", position = "dodge")
  
#Data agrupada por Cuenca----
data_cluster |> 
  filter(cluster==4) |> 
  group_by(nombre_cuenca) |> 
  count(nombre_cuenca) |> 
  ggplot(aes(x = nombre_cuenca, y = n)) + 
  geom_bar(stat = "identity", position = "dodge")
  
#reddPrec-----

data_rp <- data_1981_2020_na |> 
  mutate(valor=as.numeric(valor)) |> 
  #select(codigo_estacion, valor) |> 
  pivot_wider(names_from=codigo_estacion, values_from=valor) |> 
  select(-fecha) |> 
  as.matrix()

data_sts <- data_ubi_utm |> 
  select(codigo_estacion, altura, X, Y) |> 
  mutate(ALT=as.numeric(altura), X=as.numeric(X), Y=as.numeric(Y)) |>
  rename(ID=codigo_estacion) |> 
  select(ID, ALT, X, Y) 

inicio <- as.Date("1981-01-01")
final <- as.Date("2020-03-31")

#data("precipDataset")
data_rp_sampled <- data_rp |> 
  as.data.frame() |> 
  sample_frac(0.1) |> 
  as.matrix()

data_sts_sampled <- data_sts |> 
  sample_frac(0.1)

inicio_s <- as.Date("1981-01-01")
final_s <- inicio_S + 124
final_s |> 
  as.Date()

if (nrow(data_rp_sampled) != nrow(data_sts_sampled)) {
  stop("Los conjuntos de datos tienen cantidad de filas diferentes.")
}

qcPrec(data_rp_sampled, data_sts_sampled, inicio_s, final_s, parallel = TRUE, ncpu = 2, printmeta = TRUE, thres = NA)


 nclass(data_sts)


#Qoutliers 3 veces el rango intercuantil reddPrec












