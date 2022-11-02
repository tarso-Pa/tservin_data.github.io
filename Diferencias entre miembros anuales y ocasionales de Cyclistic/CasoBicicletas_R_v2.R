# Cargando librerías
library(tidyverse)  
library(lubridate)  
library(ggplot2)
library(dplyr)
library(lessR)

# Importando archivos csv
m1<- read_csv("202110-divvy-tripdata.csv")
m2<- read_csv("202111-divvy-tripdata.csv")
m3<- read_csv("202112-divvy-tripdata.csv")
m4<- read_csv("202201-divvy-tripdata.csv")
m5<- read_csv("202202-divvy-tripdata.csv")
m6<- read_csv("202203-divvy-tripdata.csv")
m7<- read_csv("202204-divvy-tripdata.csv")
m8<- read_csv("202205-divvy-tripdata.csv")
m9<- read_csv("202206-divvy-tripdata.csv")
m10<- read_csv("202207-divvy-tripdata.csv")
m11<- read_csv("202208-divvy-tripdata.csv")
m12<- read_csv("202209-divvy-tripdata.csv")

# Revisando nombres de columnas
colnames(m1)
colnames(m2)
colnames(m3)
colnames(m4)
colnames(m5)
colnames(m6)
colnames(m7)
colnames(m8)
colnames(m9)
colnames(m10)
colnames(m11)
colnames(m12)
# Todos los data frame tienen los mismos nombre de columna

# Vista previa y estructura
glimpse(m1) # hay campos vacíos
glimpse(m2) # hay campos vacíos
glimpse(m3)
glimpse(m4)
glimpse(m5)
glimpse(m6)
glimpse(m7)
glimpse(m8)
glimpse(m9)
glimpse(m10)
glimpse(m11)
glimpse(m12)
# Tipo de datos congruentes

# Seleccionando columnas útiles y también para disminuir el impacto en la memoria RAM
m1 <- m1 %>% select(rideable_type, started_at, ended_at, member_casual)
m2 <- m2 %>% select(rideable_type, started_at, ended_at, member_casual)
m3 <- m3 %>% select(rideable_type, started_at, ended_at, member_casual)
m4 <- m4 %>% select(rideable_type, started_at, ended_at, member_casual)
m5 <- m5 %>% select(rideable_type, started_at, ended_at, member_casual)
m6 <- m6 %>% select(rideable_type, started_at, ended_at, member_casual)
m7 <- m7 %>% select(rideable_type, started_at, ended_at, member_casual)
m8 <- m8 %>% select(rideable_type, started_at, ended_at, member_casual)
m9 <- m9 %>% select(rideable_type, started_at, ended_at, member_casual)
m10 <- m10 %>% select(rideable_type, started_at, ended_at, member_casual)
m11 <- m11 %>% select(rideable_type, started_at, ended_at, member_casual)
m12 <- m12 %>% select(rideable_type, started_at, ended_at, member_casual)


# Uniendo todos los data frame
total_trips_v2 <- bind_rows(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)

# Revisando columna, estructura y resumen
colnames(total_trips_v2)
glimpse(total_trips_v2)
summary(total_trips_v2)

# Mostrar, agrupar y ordenar de forma descendente los tipos de bicicleta
# en "rideable_type".
total_trips_v2 %>%
	group_by(rideable_type) %>%
	summarize(n = n()) %>%
  arrange(desc(n), .by_group=TRUE)
	
# Mostrar, agrupar y ordenar de forma descendente los tipos de usuario
# en "member_casual".
total_trips_v2 %>%
  group_by(member_casual) %>%
  summarize(n = n()) %>%
  arrange(desc(n), .by_group=TRUE)


# Crear nuevas columnas a partir de "started_at" y "ended_at"
# Esto nos ayudará más adelante a calcular estadísticas por estos periodos,
# en el entendido de que el mismo día de desbloqueo es el mismo del bloqueo
total_trips_v2$date <- as.Date(total_trips_v2$started_at) 
# usar esto si se quiere que el mes aparezca abreviado
total_trips_v2$month <- format(as.Date(total_trips_v2$date), "%b") # %m, número; %b, abreviado
# usar esto si la fecha sea "año-mes_abreviado". Nota, no usar por no poder ordenarse
# total_trips_v2$year_month <- format(as.Date(total_trips_v2$date), "%Y/%b")
total_trips_v2$day <- format(as.Date(total_trips_v2$date), "%d")
total_trips_v2$year <- format(as.Date(total_trips_v2$date), "%Y")
total_trips_v2$day_of_week <- format(as.Date(total_trips_v2$date), "%A")


# Calculando el tiempo de uso entre desbloqueo y bloqueo de la bicicleta: "ride_length"
total_trips_v2$ride_length <- difftime(total_trips_v2$ended_at,total_trips_v2$started_at, units = "mins")

# Calculando media, moda, mediana, máximo y mínimo, desviaciuón estándar y cuartiles de "ride_length"
mean(total_trips_v2$ride_length) 
median(total_trips_v2$ride_length) 
max(total_trips_v2$ride_length) 
min(total_trips_v2$ride_length)
sd(total_trips_v2$ride_length)
quantile(total_trips_v2$ride_length)

# Problemas detectados
# a. Hay mucho outliers por la derecha, hace que los valores de los cuartiles estén
# muy por debajo de los valores necesarios para el análisis
# b. Hay diferencias de tiempo negativas, hay que filtrar el valor mínimo a 1 minuto.

# Filtrando tiempo de viaje conforme a restricciones, mínimo 1 min, máximo 60 min.
total_trips_v2 <- total_trips_v2 %>% 
  filter(ride_length >= 1) %>% 
  filter(ride_length <= 60)

# Revisando cuartiles y boxplot
quantile(total_trips_v2$ride_length)
boxplot(total_trips_v2$ride_length)

# Calculando la última observación antes de (Q3 + 1.5RI)
# Q1 = 8.18, Q3 = 20.22, IR = Q3 - Q1
Q1 <- 6.05
Q3 <- 17.93
IR <- Q3 - Q1
out <- Q3 + (1.5 * IR)
out

# Filtrando outliers por encima de (Q3 + 1.5RI) = 35.75. 
total_trips_v2 <- total_trips_v2 %>% 
  filter(ride_length <= 35.75 )

# Revisando estadísticas
mean(total_trips_v2$ride_length) 
sd(total_trips_v2$ride_length)
quantile(total_trips_v2$ride_length)
boxplot(total_trips_v2$ride_length)

# Activar cuando sea necesario:
# Guardando cambios en el data frame en una nueva tabla
# write_csv(total_trips_v2, "total_trips_v1.csv")
# Cargando datos desde archivo csv
# total_trips <- read.csv("total_trips_v1.csv")

# Número de viajes y porcentaje por tipo de usuario y tipo de bicicleta
# ordenado descendentemente por número de viajes y grupos
total_trips_v2 %>%
  group_by(member_casual, rideable_type) %>%
  summarize(n = n(), pct = n() * 100 / nrow(total_trips_v2)) %>%
  arrange(desc(n), .by_group=TRUE)

# Número de viajes y porcentaje por tipo de usuario y tipo de bicicleta
# ordenado descendentemente por número de viajes
total_trips_v2 %>%
  group_by(member_casual, rideable_type) %>%
  summarize(n = n(), pct = n() * 100 / nrow(total_trips_v2)) %>%
  arrange(desc(n))

# Algunas medidas de tendencia central, comparando entre tipo de usuarios
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN = mean)
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN = median)
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN = max)
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual, FUN = min)

# Promedio agrupado de "ride_length" agrupado por member_casual" y "ride_length"
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual + total_trips_v2$day_of_week, FUN = mean)

# Número de viajes por día de la semana
total_trips_v2 %>%
  group_by(day_of_week) %>%
  summarize(n = n()) %>%
  arrange(day_of_week)

# Reordenando la tabla en la columna "day_of_week" en secuencia de domingo a sábado.
total_trips_v2$day_of_week <- ordered(total_trips_v2$day_of_week, levels=c("domingo", "lunes", "martes", 
                                   "miércoles", "jueves", "viernes", "sábado"))

# Revisando cambio anterior con aggregate
aggregate(total_trips_v2$ride_length ~ total_trips_v2$member_casual + total_trips_v2$day_of_week, FUN = mean)

# Tabla con medidas de tendencia central y número de viajes (n)
# agrupada por "member_casual" y "day_of_week", ordenada por "day_of_week"
total_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(average=mean(ride_length), median=median(ride_length), min=min(ride_length)
            , max=max(ride_length), number_of_rides = n()) %>% 
  arrange (day_of_week)

# Tabla con medidas de tendencia central y número de viajes (n)
# agrupada por "member_casual" y "rideable_type", ordenada por "rideable_type"
total_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(average=mean(ride_length), median=median(ride_length), min=min(ride_length)
            , max=max(ride_length), number_of_rides = n()) %>% 
  arrange (rideable_type)

# Tabla con medidas de tendencia central y número de viajes (n)
# agrupada por "rideable_type" y "member_casual", ordenada por "member_casual"
total_trips_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(average=mean(ride_length), median=median(ride_length), min=min(ride_length)
            , max=max(ride_length), number_of_rides = n()) %>% 
  arrange (member_casual)

# Gráfico de pie 1: número de viajes por tipo de usuario
table_users_type <- table(total_trips_v2$member_casual)
table_users_type 
PieChart(table_users_type, hole = 0, values = "%", fill = c("darkorange", "darkgreen")
         , main = "Porcentaje de viajes por tipo de usuario", values_size = 2)

# Convirtiendo "ride_length" de formato difftime a tipo numérico para hacer el histograma

class(total_trips_v2$ride_length)
total_trips_v2$ride_length_num <- as.numeric(total_trips_v2$ride_length)
total_trips_v2$ride_length_num

# Histograma + plotbox 1: variable "ride_length" 
hist(x = total_trips_v2$ride_length_num
     , main = "Histograma de tiempo de viaje\noctubre 2021 - septiembre 2022"
     , xlab = "minutos"
     , ylab = "frecuencia", col = "lightblue")
axis(1) #añade eje x
par(new = TRUE)
boxplot(total_trips_v2$ride_length_num, horizontal = TRUE, axes = FALSE, lwd = 2
        , col = rgb(0, 0, 0, alpha = 0.2))

# Boxplot 1: variable "ride_length" por usuario, "member_casual"
boxplot(total_trips_v2$ride_length ~ total_trips_v2$member_casual
        , main = "Boxplot: tiempo de viaje por tipo de usuario"
        , xlab = "Tipo de usuario"
        , ylab = "Tiempo en minutos")

# Boxplot 2: variable "ride_length" por tipo de bicicleta, "rideable_type"
boxplot(total_trips_v2$ride_length ~ total_trips_v2$rideable_type
        , main = "Boxplot: tiempo de viaje por tipo de bicicleta"
        , xlab = "Tipo de bicicleta"
        , ylab = "Tiempo en minutos") 


# Reordenando la tabla en la columna "day_of_week" en secuencia de domingo a sábado.
total_trips_v2$month <- ordered(total_trips_v2$month
                                , levels=c("oct", "nov", "dic", "ene", "feb", "mar"
                                           , "abr", "may","jun", "jul", "ago", "sep"))

# Gráfico de barras 1: combinando "month" con "member_casual" del número de viajes
# Nota: los meses tienen formato abreviado. Los meses oct, nov y dic son del año 2021, 
# los restantes corresponden al 2022

total_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(month, member_casual) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title = "Gráfico mensual del número de viajes por tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "mes", y = "número de viajes" ) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45))

# Gráfico de barras 2: 
#Similar al gráfico anterior. En lugar del número de viajes, el eje de la "y" es para el 
# promedio de duración de viaje. Gráfico de barras NO apiladas con variables agrupadas.
# Los meses de octubre a diciembre son del 2021, lo restantes corresponden al 2022
total_trips_v2 %>%
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  labs(title = "Gráfico mensual del tiempo en promedio de viaje por tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "mes", y = "tiempo promedio en minutos" ) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45))

# Gráfico de barras 3: combinando "month" con "rideable_type" del número de viajes
# Nota: los meses tienen formato de caracter. Los meses 10 al 12 son del año 2021, 
# los restantes corresponden al 2022
ggplot(data = total_trips_v2) +
  geom_bar(mapping = aes(x = month, fill = rideable_type)) +
  labs(title = "Gráfico mensual del número de viajes por tipo de usuario\ny tipo de bicicleta"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "mes", y = "número de viajes" ) +
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 45))

# Gráfico de barras 4: apilamiento combinando "day_of_week" con "member_casual" del número de viajes
# en facetas de tipo de bicicleta
ggplot(data = total_trips_v2) +
  geom_bar(mapping = aes(x = day_of_week, fill = member_casual)) +
  labs(title = "Gráfico del número de viajes agrupado por\nel día de la semana, tipo de bicicleta y usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "número de viajes" ) +
  facet_wrap(~rideable_type) +
  theme(axis.text.x = element_text(angle = 45))

# Gráfico de barras 5: apilamiento combinando "rideable_type" con "member_casual" del número de viajes
ggplot(data = total_trips_v2) +
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual)) +
  labs(title = "Gráfico del número de viajes por tipo de bicicleta y tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "Tipo de bicicleta", y = "número de viajes" )
  
# Gráfico de barras 6: apilamiento combinando "day_of_week" con "rideable_type" del número de viajes
ggplot(data = total_trips_v2) +
  geom_bar(mapping = aes(x = day_of_week, fill = rideable_type)) +
  labs(title = "Gráfico del número de viaje por día de la semana y tipo de bicicleta"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "número de viajes" )

# Tabla de barras
# analyze ridership data by type and day_of_week
total_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>%  
  summarise(number_of_rides = n() #calculates the number of rides and average duration, 
            ,average_duration = mean(ride_length)) %>%    # calculates the average duration
  arrange(member_casual, day_of_week)                     # sorts

# Gráfico de barras 7
# Let's visualize the number of rides by rider type + facet(member_casual)
total_trips_v2 %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title = "Gráfico del número de viajes por día de la semana,\ntipo de bicicleta y tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "número de viajes" ) +
  facet_wrap(~rideable_type) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(position = "dodge")

# Gráfico de barras 8
# Let's visualize the number of rides by rider type + facet(member_casual)
total_trips_v2 %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = rideable_type)) +
  labs(title = "Gráfico del número de viajes por día de la semana,\ntipo de usuario y tipo de bicicleta"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "número de viajes" ) +
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(position = "dodge")


# Gráfico de barras 9
# Let's create a visualization for average duration
total_trips_v2 %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  labs(title = "Gráfico del tiempo promedio de viaje por día de la semana,\ntipo de bicicleta y tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "tiempo promedio en minutos" ) +
  facet_wrap(~rideable_type) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(position = "dodge")

# Gráfico de barras 10
# Intentemos una faceta: day_of_week, member_casual y como facetas rideable_type
ggplot(data = total_trips_v2) +
  geom_bar(mapping = aes(x = day_of_week, fill = member_casual)) +
  labs(title = "Gráfico de barras apiladas,\nnúmero de viajes por día de la semana por tipo de bicicleta y tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "número de viajes" ) +
  facet_wrap(~rideable_type) +
  theme(axis.text.x = element_text(angle = 45)) 






# Gráficos para corregir

# Gráfico de barras 5: apilamiento combinando "rideable_type" con "member_casual" del número de viajes
ggplot(data = total_trips_v2) +
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual)) +
  labs(title = "Gráfico del número de viajes por tipo de bicicleta y tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "Tipo de bicicleta", y = "número de viajes" )


# Gráficos adicionales versión tiempo promedio

# Gráfico de barras 11:  combinando "month" con "rideable_type"  y "member_casual # del tiempo promedio de viaje
# Nota: los meses tienen formato de caracter. Los meses 10 al 12 son del año 2021, los restantes corresponden al 2022
total_trips_v2 %>% 
  group_by(member_casual, month, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = rideable_type)) +
  labs(title = "Gráfico del tiempo promedio de viaje por día de la semanames,\ntipo de bicicleta y tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "mes", y = "tiempo promedio en minutos" ) +
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(position = "dodge")


# Gráfico de barras 12: apilamiento combinando "day_of_week" con "rideable_type" del tiempo promedio de viaje
total_trips_v2 %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = rideable_type)) +
  labs(title = "Gráfico del tiempo promedio de viaje por día de la semana,\ny tipo de bicicleta"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "tiempo promedio en minutos" ) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(position = "dodge")

# Gráfico de barras 13
# Let's visualize the "average_duration" by rider type + facet(member_casual)
total_trips_v2 %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = rideable_type)) +
  labs(title = "Gráfico del tiempo promedio de viaje por día de la semana,\ny tipo de bicicleta y tipo de usuario"
       , subtitle = "octubre 2021 - septiembre 2022"
       , x = "día de la semana", y = "tiempo promedio en minutos" ) +
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(position = "dodge")




