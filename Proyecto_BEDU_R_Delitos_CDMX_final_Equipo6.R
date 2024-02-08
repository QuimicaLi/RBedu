library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)


setwd("C:/Users/User/Downloads")
# Leer datos en un Dataframe
delitos.data<-delitos.data<-carpetasFGJ_acumulado_2023_12
alcaldias <- read_sf("alcaldias.geojson")

# Estadísticas y descripción de nuestras variables
summary(delitos.data)
str(delitos.data)

# Ejemplos de filas
head(delitos.data)

Nombres_de_columnas <- colnames(delitos.data)

#Contar filas con información incompleta/completa
# Na's por columna
apply(X = is.na(delitos.data), MARGIN = 2, FUN = sum)
# Filas sin información faltante
Complete_cases_delitos <- complete.cases(delitos.data)
sum(Complete_cases_delitos)

Nombres_de_columnas <- colnames(delitos.data())
delitos.data <- select(delitos.data,-c(anio_inicio:anio_hecho, competencia, ))
# Filtro Filas completas (opcional) 

delitos.data[Complete_cases_delitos,]

# Número de delitos distintos
n_distinct(delitos.data$delito)
n_distinct(delitos.data$fiscalia)

# ¿Cuántos delitos registrados hay en la tabla? ¿Qué rango de tiempo consideran los datos?
# Se hizo anteriormente con str(delitos.data)

dim(delitos.data)

# 1848792 delitos; Aquí calculamos el rango de fechas para los delitos que cuentan con este dato. 

fecha <- na.omit(as.Date(delitos.data$fecha_hecho, "%Y-%m-%d"))
range(fecha)
#____________________________________________________________________________________________________________________
fecha_2 <- na.omit(as.Date(delitos.data$fecha_inicio, "%Y-%m-%d"))
range(fecha_2)

fecha_2 <- as.Date(delitos.data$fecha_inicio, "%Y-%m-%d")

# rango de datos fecha "2016-01-01" "2023-12-31"

# ¿Cuáles son los 8 delitos más frecuentes?

delitos.data <- delitos.data %>% mutate(fecha = ymd(fecha_2))

delitos_frecuentes <- delitos.data %>% 
  count(delito) %>% 
  top_n(8) %>% 
  pull(delito)


delitos.data %>% 
  filter(year(fecha_2) <= 2023,
         delito %in% delitos_frecuentes) %>% 
  ggplot() +
  geom_bar(aes(x = month(fecha, label = TRUE), fill = delito))

# Histograma acumulado 8 delitos más frecuentes

delitos.data %>% 
  filter(year(fecha_2) <= 2023,
         delito %in% delitos_frecuentes) %>% 
  ggplot() +
  geom_bar(aes(x = delito))

# Histograma 8 delitos más frecuentes en 2023

delitos.data %>% 
  filter(year(fecha_2) == 2023,
         delito %in% delitos_frecuentes) %>% 
  ggplot() +
  geom_bar(aes(x = delito))

# Analizamos algunos de los delitos más frecuentes, para identificar tendencia de alza o baja con una serie de tiempo.

fecha_3 <- as.Date(delitos.data$fecha_hecho, "%Y-%m-%d")

delitos.data <- delitos.data %>% mutate(fecha_2023 = ymd(fecha_3))
delitos_anio_anterior <- filter(delitos.data, year(fecha_2023) == 2023)
delitos_anio_anterior_filtrado <- filter(delitos_anio_anterior, delito %in% delitos_frecuentes) 
conteo_Mes <-  delitos_anio_anterior_filtrado %>% 
  count(delito, mes = month(fecha_2023), label = TRUE)

ggplot(conteo_Mes) +
  geom_line(aes(x = mes, y = n, group = delito, color = delito))

# Identificamos este delito a la alza

delitos_anio_anterior_RTVP <- filter(delitos.data, delito == "ROBO DE ACCESORIOS DE AUTO")

conteo_Mes <-  delitos_anio_anterior_RTVP %>% 
  count(delito, date1 = fecha_2023, label = TRUE)

# 1
ggplot(conteo_Mes) +
  geom_line(aes(x = date1, y = n, group = delito, color = delito)) +
  coord_cartesian(xlim = c(as.Date("2016-1-1"),as.Date("2023-12-31")), ylim = c(0,50))
# 2 
ggplot(conteo_Mes) +
  geom_line(aes(x = date1, y = n, group = delito, color = delito)) +
  coord_cartesian(xlim = c(as.Date("2023-1-1"),as.Date("2023-12-31")), ylim = c(0,50))

# ¿Cuál es la alcaldía que más delitos tiene y cuál es la que menos en el acumulado?

delitos_alcaldia <- delitos.data %>% count(alcaldia_catalogo)

delitos_alcaldia <- delitos_alcaldia[with(delitos_alcaldia, order(-n)), ]
delitos_alcaldia_ordenado <- arrange(delitos_alcaldia,-n)

# barplot(height = delitos_alcaldia_ordenado$n, names.arg=delitos_alcaldia_ordenado$alcaldia_catalogo, las = 2) # estilo________

delitos_alcaldia_ordenado_1 <- alcaldias %>%
  left_join(delitos_alcaldia_ordenado, by = c("NOMGEO" = "alcaldia_catalogo"))


ggplot(delitos_alcaldia_ordenado_1) +
  geom_sf(aes(fill = n),color = "white",
          linetype = 1,
          lwd = 0.25) +
  scale_fill_viridis_c(direction = -1) +
  geom_sf_text(aes(label = NOMGEO), size = 2, fun.geometry = sf::st_centroid,  colour = "white")

#fontface = "bold"   

#data <- read_csv("carpetasFGJ_acumulado_2023_12.csv")
#my_image <- readPNG("mapa.png")

del_name <- "ROBO DE ACCESORIOS DE AUTO"
loc_del <- delitos.data %>%
  dplyr::filter(anio_hecho==2023, delito == del_name) %>%
  dplyr::select(, c("latitud", "longitud")) %>%
  na.omit()

plot(y=loc_del$latitud, x=loc_del$longitud, main = 
       "Localización de Robos de Accesorios de Auto en CDMX 2023 / ", 
     xlab = "Longitud", ylab = "Latitud")

kmeans_res <- kmeans(loc_del, 100, iter.max = 1000, nstart = 15)
points(y = kmeans_res$centers[,1], x = kmeans_res$centers[,2], 
       col="red", pch=19)

