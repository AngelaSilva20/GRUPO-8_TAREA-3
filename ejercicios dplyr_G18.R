install.packages("nycflights13")
library(dplyr)
library(nycflights13)
library(tidyverse)

nycflights13::flights
flights
View(flights)

#9.1 Parte 1:Dplyr - filter
#1.Encuentra todos los vuelos que:
#A.tuvieron un retraso de llegada de dos o más horas:
v_retraso <- filter(flights, arr_delay >= 120 )
View(v_retraso)
v_retraso

#B.Volaron a Houston(IAH o HOU)
filter(flights, dest == "IAH" | dest == "HOU")


#C.Fueron operados por United, American o Delta

filter(flights, carrier == "USA" | carrier == "AA" | carrier == "DL")


#D.Encuentra todos los vuelos que partieron en invierno del hemisferio sur (julio, agosto, septiembre)

filter(flights, month == 7 |  month == 8 | month == 9)


#E.Encuentra todos los vuelos que llegaron más de dos horas tarde, pero no salieron tarde.

filter(flights, arr_delay > 120, dep_delay <=0)


#F.Se retrasaron por lo menos una hora, pero repusieron más de 30 minutos en vuelo.

filter(flights, dep_delay >= 60, arr_delay < 30)


#G.Partieron entre la medianoche y las 6 a.m.

filter(flights, dep_time >= 0 & dep_time <= 600)


#2.Otra función de DPLYR que es útil para usar filtros es between().¿Qué hace? ¿Puedes usarla para simplificar el código necesario para responder a los desafíos anteriores? 

# Nos Devuelve un valor lógico que indica si el valor especificado está dentro de un rango.
# Para la pregunta D: vuelos que partieron en invierno del hemisferio sur (julio, agosto, septiembre)

filter(flights, between(month, 7,9))

#3.¿Cuántos vuelos tienen datos faltantes en horario_salida? ¿Qué otras variables tienen valores faltantes? ¿Qué representan estas filas?

# ¿Cuántos vuelos tienen datos faltantes en horario_salida?

View(count((filter(flights, is.na(dep_time)))))

# ¿Qué otras variables tienen valores faltantes?

View(filter(flights, is.na(dep_time)))
## dep_delay, arr_time, arr_delay, tailnum, air_time 


# ¿Qué representan estas filas?
## Las filas con datos faltantes(NA) representan los vuelos que fueron cancelados. Ya que al ser cancelado, se entiende que no hay datos de de retraso en salida o en llegada del vuelo.

# 9.2 Parte 2: Dplyr - arrange
## 1 ¿Cómo podrías usar arrange() para ordenar todos los valores faltantes al comienzo? (Sugerencia: usa is.na())

arrange(flights,is.na(air_time))

## 2 Ordena vuelos para encontrar los vuelos más retrasados. Encuentra los vuelos que salieron más temprano. 

View(arrange(flights, desc(dep_delay)))
View(arrange(flights, dep_delay > 0))

## 3 Ordena vuelos para encontrar los vuelos más rápidos (que viajaron a mayor velocidad). 

vuelos02 <- arrange( flights, desc(distance / air_time))
head(vuelos02)

## 4 ¿Cuáles vuelos viajaron más lejos? ¿Cuál viajó más cerca? 
## ¿Cuáles vuelos viajaron más lejos?

mas_lejos <- arrange(flights, desc(distance))
mas_lejos

## ¿Cuál viajó más cerca?

mas_cerca <- arrange(flights, distance)
mas_cerca

# Parte 3: Dplyr - select 

## 1 Haz una lluvia de ideas sobre tantas maneras como sea posible para seleccionar dep_time, dep_delay, arr_time, and arr_delay de flights. 

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("carrier"), starts_with("time"))


## 2 ¿Qué sucede si incluyes el nombre de una variable varias veces en una llamada a select()?

select(flights, dep_delay, arr_time, dep_delay, carrier, carrier)


## 3 ¿Qué hace la función any_of()? ¡¿Por qué podría ser útil en conjunto con este vector? 
# ¿Qué hace la función any_of()?

### La función any_of() al igual que la función all_of()coinciden con nombres de variables en un vector de caracteres, en all_of()todos los nombres deben de estar presentes de lo contrario , se genera un error de fuera de límites ; en cambio any_of() no arroja ningún error para los nombres que no existen.

# ¿Por qué podría ser útil en conjunto con este vector?

### La función any_of() ayuda a dbplyr a que se implemente un dialecto de R donde los operadores facilitan la selección de variables , en este caso por medio del vector ; estos ayudantes seleccionan variables de un vector de caracteres.


# Parte 4: Dplyr - mutate
## 1 Las variables horario_salida y salida_programada tienen un formato conveniente para leer, pero es difícil realizar cualquier cálculo con ellas porque no son realmente números continuos. Transfórmalas hacia un formato más conveniente como número de minutos desde la medianoche.
# dep_time

vuelosABC <- select(flights,
                    year:month,
                    starts_with("dep_time"))
mutate(vuelosABC,
       hora = dep_time %/% 100,
       minuto = dep_time %% 100)              

transmute(vuelosABC,
          hora = dep_time %/% 100,
          minuto = dep_time %% 100)


# sched_dep_time

vuelosABC <- select(flights,
                    year:month,
                    starts_with("sched_dep_time"))
mutate(vuelosABC,
       hora = sched_dep_time %/% 100,
       minuto = sched_dep_time %% 100)              

transmute(vuelosABC,
          hora = sched_dep_time %/% 100,
          minuto = sched_dep_time %% 100)

## 2 Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué esperas ver? ¿Qué ves? ¿Qué necesitas hacer para arreglarlo?

vuelos01 <- select(flights, dep_time, arr_time)

mutate(flights, 
       tiempo_vuelo = arr_time - dep_time)

## 3 Compara horario_salida, salida_programada, y atraso_salida. ¿Cómo esperarías que esos tres números estén relacionados?

#Podemos indicar que el atraso_salida = horario_salida - salida_programada

select(flights, dep_time, sched_dep_time, dep_delay)
transmute(flights, tiempo_vuel0 = dep_time - sched_dep_time)

## 4 Encuentra los 10 vuelos más retrasados utilizando una función de ordenamiento. ¿Cómo quieres manejar los empates? Lee atentamente la documentación de min_rank().

vuelos_retraso <-head(arrange(flights,desc(arr_delay)),10)
vuelos_retraso

vuelos_10<-vuelos_retraso$arr_delay
vuelos_10

min_rank(vuelos_10)

## 5 ¿Qué devuelve 1:3 + 1:10? ¿Por qué?

1:3 + 1:10

#Nos devuelve que la longitud del objeto más largo(1:10) no es un múltiplo de la longitud del objeto más corto(1:3)

## 6 ¿Qué funciones trigonométricas proporciona R?

# R proporciona las funciones coseno, tangente y cotangente

sin(pi/3)
cos(pi)
tan(pi/2)
1/tan(pi)


# Parte 5_ Dplyr - group by & summarize 

## 1 Haz una lluvia de ideas de al menos 5 formas diferentes de evaluar las características de un retraso típico de un grupo de vuelos. Considera los siguientes escenarios:

### A) Un vuelo llega 15 minutos antes 50% del tiempo, y 15 minutos tarde 50% del tiempo. 


### B) Un vuelo llega siempre 10 minutos tarde.


### C) Un vuelo llega 30 minutos antes 50% del tiempo, y 30 minutos tarde 50% del tiempo.


### D) Un vuelo llega a tiempo en el 99% de los casos. 1% de las veces llega 2 horas tarde. ¿Qué es más importante: retraso de la llegada o demora de salida?


## 2 Sugiere un nuevo enfoque que te dé el mismo output que no_cancelados %>% count(destino) y no_cancelado %>% count(codigo_cola, wt = distancia) (sin usar count()).



## 3 Nuestra definición de vuelos cancelados (is.na(atraso_salida) | is.na (atraso_llegada)) es un poco subóptima. ¿Por qué? ¿Cuál es la columna más importante?
#por definición
no_cancelados <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
no_cancelados

#otra forma
no_cancelados %>% 
  group_by(year, month, day) %>% 
  summarise(media = mean(dep_delay))

#La columna mas importante es el promedio de vuelos no cancelados

## 4 Mira la cantidad de vuelos cancelados por día. ¿Hay un patrón? ¿La proporción de vuelos cancelados está relacionada con el retraso promedio?

por_dia <- group_by(flights, year, month, day)
summarise(por_dia, retraso = mean(dep_delay, na.rm = TRUE))

## 5 ¿Qué compañía tiene los peores retrasos? Desafío: ¿puedes desenredar el efecto de malos aeropuertos vs. el efecto de malas aerolíneas? ¿Por qué o por qué no? (Sugerencia: piensa en vuelos %>% group_by(aerolinea, destino) %>% summarise(n()))


## 6 ¿Qué hace el argumento sort a count(). ¿Cuándo podrías usarlo?
##La función count() nos permite saber cuantas observaciones hay en una variable especifica. Al agregar el argumento sort = TRUEdevuelve una tabla descendiente con el número de observaciones.


# Parte 7: Dplyr - transformaciones agrupadas

## 1 Remítete a las listas de funciones útiles de mutación y filtrado. Describe cómo cambia cada operación cuando las combinas con la agrupación.


## 2 ¿Qué avión (codigo_cola) tiene el peor registro de tiempo?


## 3 ¿A qué hora del día deberías volar si quieres evitar lo más posible los retrasos?


## 4 Para cada destino, calcula los minutos totales de demora. Para cada vuelo, calcula la proporción de la demora total para su destino.


## 5 Los retrasos suelen estar temporalmente correlacionados: incluso una vez que el problema que causó el retraso inicial se ha resuelto, los vuelos posteriores se retrasan para permitir que salgan los vuelos anteriores. Usando lag(), explora cómo el retraso de un vuelo está relacionado con el retraso del vuelo inmediatamente anterior.
 

## 6 Mira cada destino. ¿Puedes encontrar vuelos sospechosamente rápidos? (es decir, vuelos que representan un posible error de entrada de datos). Calcula el tiempo en el aire de un vuelo relativo al vuelo más corto a ese destino. ¿Cuáles vuelos se retrasaron más en el aire?


## 7 Encuentra todos los destinos que son volados por al menos dos operadores. Usa esta información para clasificar a las aerolíneas.


## 8 Para cada avión, cuenta el número de vuelos antes del primer retraso de más de 1 hora. 















                                             