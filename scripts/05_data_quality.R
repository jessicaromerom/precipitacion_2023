library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(reddPrec)


data_estaciones_utm <- read_rds('data/data_procesada/data_estac_utm.rds')
data_1981_2020_na_prop <- read_rds('data/data_procesada/data_1981_2020_na_prop.rds')
data_ubi_utm <- left_join(data_1981_2020_na_prop, data_estaciones_utm, by = 'codigo_estacion') 
data_1981_2020_na <- read_rds('data/data_procesada/data_1981_2020_na.rds')

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
  arrange(codigo_estacion) |> 
  mutate(valor=as.numeric(valor)) |> 
  #select(codigo_estacion, valor) |> 
  pivot_wider(names_from=codigo_estacion, values_from=valor) |> 
  select(-fecha) |> 
  as.matrix()

data_sts <- data_ubi_utm |> 
  arrange(codigo_estacion) |> 
  select(codigo_estacion, altura, X, Y) |> 
  mutate(ALT=as.numeric(altura), X=as.numeric(X), Y=as.numeric(Y)) |>
  rename(ID=codigo_estacion) |> 
  select(ID, ALT, X, Y) 

inicio <- as.Date("1981-01-01")
final <- as.Date("2020-03-31")

#data("precipDataset")

ids <- sample(1:1254,10)

data_rp_sampled <- data_rp[,ids]
st_selec <- attributes(data_rp)[[2]][[2]][ids]

# data_rp_sampled <- data_rp |> 
#   as.data.frame() |> 
#   sample_frac(0.1) |> 
#   as.matrix()

data_sts_sampled <- data_sts[ids,]

# data_sts_sampled <- data_sts |> 
#   sample_frac(0.1)

inicio_s <- as.Date("1981-01-01")
final_s <- inicio_s + 124
final_s |> 
  as.Date()

if (nrow(data_rp_sampled) != nrow(data_sts_sampled)) {
  stop("Los conjuntos de datos tienen cantidad de filas diferentes.")
}

qcPrec(data_rp_sampled, data_sts_sampled, inicio, final, parallel = TRUE, ncpu = 5, printmeta = TRUE, thres = NA)


class(data_sts)


#Qoutliers Paper----
#outliers_prop

#funcion de Q (calidad indidual de 0 a 100)
calc_Q <- function(P, ngap, Lmaxgap, CV, outliers_prop) {
  Q <- (1/4) * (P + (100 - 100/(2*ngap + Lmaxgap/n)) + (100 - 100(CV)) + (100 - outliers_prop))
  return(Q)
}


P <- ndays / n

#Qgaps
ngap <- ngap #n dias sin datos del anio
Lmaxgap <- Lmaxgap # longitud maxima de un periodo vacio sin datos

#Qwzeros
rainy_days <- c(n_mon, n_tue, n_wed, n_thu, n_fri, n_sat, n_sun) #numero de dias de precipitacion de la semana
CV <- sd(rainy_days) / mean(rainy_days) #coeficiente de variacion para cada anio

n_estacion<-data_1981_2020_na |> 
  filter(codigo_estacion == '09417002') |>
  select(fecha, valor) |> 
  mutate(fecha = as.Date(fecha), valor =  as.numeric(valor)) |> 
  glimpse()

#Qoutliers

Qout <- apply(matrix(n_estacion$valor, ncol = 30, byrow = TRUE), 2, function(x) quantile(x, probs = 0.75))

class(n_estacion)
Qout <- n_estacion$valor
IQR(Qout, na.rm=TRUE)

#IQR rango intercuartil
#tabla con la mediana y el IQR para cada estación y cada año
IQR <- data_1981_2020_na %>% 
  group_by(codigo_estacion, year(fecha)) %>% 
  summarize(mediana = median(valor, na.rm = TRUE),
            iqr = IQR(valor, na.rm = TRUE)) |> 
  glimpse()





#Calcula el número de días de precipitación disponibles para cada año
n_days <- aggregate(fecha ~ year(fecha), data = n_estacion, FUN = function(x) sum(!is.na(x)))

P <- n_days$fecha / 365 * 100 #porcentaje de dias con valor de precipitacion disponible


#Calcula el número de días sin precipitación (ngap) y la longitud máxima de un período sin datos (Lmaxgap)
gap_stats <- aggregate(fecha ~ year(fecha), data = n_estacion, FUN = function(x) {
  ngap <- sum(is.na(x))
  if (ngap > 0) {
    gap_lengths <- rle(is.na(x))
    Lmaxgap <- max(gap_lengths$lengths[gap_lengths$values])
  } else {
    Lmaxgap <- 0
  }
  data.frame(ngap = ngap, Lmaxgap = Lmaxgap)
})

#Calcula el término Qgaps para cada año utilizando la fórmula del paper
Qgaps <- 100 - 100 * (2 * gap_stats$ngap + gap_stats$Lmaxgap) / 365

#Calcula el número de días de precipitación para cada día de la semana (ni) y el coeficiente de variación (CV) para cada año utilizando la siguiente función


weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

week_stats <- aggregate(fecha ~ year(fecha), data = n_estacion, FUN = function(x) {
  x$date <- as.Date(x$fecha)
  daily_precip <- tapply(x$fecha, format(x$date, "%u"), function(x) sum(!is.na(x)))
  ni <- numeric(length(weekdays))
  names(ni) <- weekdays
  ni[names(daily_precip)] <- daily_precip
  CV <- sd(ni) / mean(ni)
  data.frame(ni = ni, CV = CV)
})

#Calcula el término Qwzero para cada año
Qwzero <- 100 - 100 * week_stats$CV

#outliers_threshold <- apply(matrix(your_data$Precipitation, ncol = 30, byrow = TRUE), 2, function(x) quantile(x, probs = c(0.25, 0.






