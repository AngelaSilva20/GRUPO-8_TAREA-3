install.packages("nycflights13")
library(dplyr)
library(nycflights13)
library(tidyverse)

nycflights13::flights
flights
View(flights)

#9.1 Parte 1:Dplyr - filter
#1.Encuentra todos los vuelos que:
#A.tuvieron un retraso de llegada de dos o m�s horas:
v_retraso <- filter(flights, arr_delay >= 120 )
View(v_retraso)
v_retraso

#B.Volaron a Houston(IAH o HOU)
filter(flights, dest == "IAH" | dest == "HOU")


#C.Fueron operados por United, American o Delta

filter(flights, carrier == "USA" | carrier == "AA" | carrier == "DL")


#D.Encuentra todos los vuelos que partieron en invierno del hemisferio sur (julio, agosto, septiembre)

filter(flights, month == 7 |  month == 8 | month == 9)


#E.Encuentra todos los vuelos que llegaron m�s de dos horas tarde, pero no salieron tarde.

filter(flights, arr_delay > 120, dep_delay <=0)


#F.Se retrasaron por lo menos una hora, pero repusieron m�s de 30 minutos en vuelo.

filter(flights, dep_delay >= 60, arr_delay < 30)


#G.Partieron entre la medianoche y las 6 a.m.

filter(flights, dep_time >= 0 & dep_time <= 600)


#2.Otra funci�n de DPLYR que es �til para usar filtros es between().�Qu� hace? �Puedes usarla para simplificar el c�digo necesario para responder a los desaf�os anteriores? 

# Nos Devuelve un valor l�gico que indica si el valor especificado est� dentro de un rango.
# Para la pregunta D: vuelos que partieron en invierno del hemisferio sur (julio, agosto, septiembre)

filter(flights, between(month, 7,9))

#3.�Cu�ntos vuelos tienen datos faltantes en horario_salida? �Qu� otras variables tienen valores faltantes? �Qu� representan estas filas?

# �Cu�ntos vuelos tienen datos faltantes en horario_salida?

View(count((filter(flights, is.na(dep_time)))))

# �Qu� otras variables tienen valores faltantes?

View(filter(flights, is.na(dep_time)))
## dep_delay, arr_time, arr_delay, tailnum, air_time 


# �Qu� representan estas filas?
## Las filas con datos faltantes(NA) representan los vuelos que fueron cancelados. Ya que al ser cancelado, se entiende que no hay datos de de retraso en salida o en llegada del vuelo.

# 9.2 Parte 2: Dplyr - arrange
## 1 �C�mo podr�as usar arrange() para ordenar todos los valores faltantes al comienzo? (Sugerencia: usa is.na())

arrange(flights,is.na(air_time))

## 2 Ordena vuelos para encontrar los vuelos m�s retrasados. Encuentra los vuelos que salieron m�s temprano. 

View(arrange(flights, desc(dep_delay)))
View(arrange(flights, dep_delay > 0))

## 3 Ordena vuelos para encontrar los vuelos m�s r�pidos (que viajaron a mayor velocidad). 

vuelos02 <- arrange( flights, desc(distance / air_time))
head(vuelos02)

## 4 �Cu�les vuelos viajaron m�s lejos? �Cu�l viaj� m�s cerca? 
## �Cu�les vuelos viajaron m�s lejos?

mas_lejos <- arrange(flights, desc(distance))
mas_lejos

## �Cu�l viaj� m�s cerca?

mas_cerca <- arrange(flights, distance)
mas_cerca

# Parte 3: Dplyr - select 

## 1 Haz una lluvia de ideas sobre tantas maneras como sea posible para seleccionar dep_time, dep_delay, arr_time, and arr_delay de flights. 

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("carrier"), starts_with("time"))


## 2 �Qu� sucede si incluyes el nombre de una variable varias veces en una llamada a select()?

select(flights, dep_delay, arr_time, dep_delay, carrier, carrier)


## 3 �Qu� hace la funci�n any_of()? ��Por qu� podr�a ser �til en conjunto con este vector? 
# �Qu� hace la funci�n any_of()?

### La funci�n any_of() al igual que la funci�n all_of()coinciden con nombres de variables en un vector de caracteres, en all_of()todos los nombres deben de estar presentes de lo contrario , se genera un error de fuera de l�mites ; en cambio any_of() no arroja ning�n error para los nombres que no existen.

# �Por qu� podr�a ser �til en conjunto con este vector?

### La funci�n any_of() ayuda a dbplyr a que se implemente un dialecto de R donde los operadores facilitan la selecci�n de variables , en este caso por medio del vector ; estos ayudantes seleccionan variables de un vector de caracteres.


# Parte 4: Dplyr - mutate
## 1 Las variables horario_salida y salida_programada tienen un formato conveniente para leer, pero es dif�cil realizar cualquier c�lculo con ellas porque no son realmente n�meros continuos. Transf�rmalas hacia un formato m�s conveniente como n�mero de minutos desde la medianoche.
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

## 2 Compara tiempo_vuelo con horario_llegada - horario_salida. �Qu� esperas ver? �Qu� ves? �Qu� necesitas hacer para arreglarlo?

vuelos01 <- select(flights, dep_time, arr_time)

mutate(flights, 
       tiempo_vuelo = arr_time - dep_time)

## 3 Compara horario_salida, salida_programada, y atraso_salida. �C�mo esperar�as que esos tres n�meros est�n relacionados?

#Podemos indicar que el atraso_salida = horario_salida - salida_programada

select(flights, dep_time, sched_dep_time, dep_delay)
transmute(flights, tiempo_vuel0 = dep_time - sched_dep_time)

## 4 Encuentra los 10 vuelos m�s retrasados utilizando una funci�n de ordenamiento. �C�mo quieres manejar los empates? Lee atentamente la documentaci�n de min_rank().

vuelos_retraso <-head(arrange(flights,desc(arr_delay)),10)
vuelos_retraso

vuelos_10<-vuelos_retraso$arr_delay
vuelos_10

min_rank(vuelos_10)

## 5 �Qu� devuelve 1:3 + 1:10? �Por qu�?

1:3 + 1:10

#Nos devuelve que la longitud del objeto m�s largo(1:10) no es un m�ltiplo de la longitud del objeto m�s corto(1:3)

## 6 �Qu� funciones trigonom�tricas proporciona R?

# R proporciona las funciones coseno, tangente y cotangente

sin(pi/3)
cos(pi)
tan(pi/2)
1/tan(pi)


# Parte 5_ Dplyr - group by & summarize 

## 1 Haz una lluvia de ideas de al menos 5 formas diferentes de evaluar las caracter�sticas de un retraso t�pico de un grupo de vuelos. Considera los siguientes escenarios:

### A) Un vuelo llega 15 minutos antes 50% del tiempo, y 15 minutos tarde 50% del tiempo. 


### B) Un vuelo llega siempre 10 minutos tarde.


### C) Un vuelo llega 30 minutos antes 50% del tiempo, y 30 minutos tarde 50% del tiempo.


### D) Un vuelo llega a tiempo en el 99% de los casos. 1% de las veces llega 2 horas tarde. �Qu� es m�s importante: retraso de la llegada o demora de salida?


## 2 Sugiere un nuevo enfoque que te d� el mismo output que no_cancelados %>% count(destino) y no_cancelado %>% count(codigo_cola, wt = distancia) (sin usar count()).



## 3 Nuestra definici�n de vuelos cancelados (is.na(atraso_salida) | is.na (atraso_llegada)) es un poco sub�ptima. �Por qu�? �Cu�l es la columna m�s importante?
#por definici�n
no_cancelados <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
no_cancelados

#otra forma
no_cancelados %>% 
  group_by(year, month, day) %>% 
  summarise(media = mean(dep_delay))

#La columna mas importante es el promedio de vuelos no cancelados

## 4 Mira la cantidad de vuelos cancelados por d�a. �Hay un patr�n? �La proporci�n de vuelos cancelados est� relacionada con el retraso promedio?

por_dia <- group_by(flights, year, month, day)
summarise(por_dia, retraso = mean(dep_delay, na.rm = TRUE))

## 5 �Qu� compa��a tiene los peores retrasos? Desaf�o: �puedes desenredar el efecto de malos aeropuertos vs. el efecto de malas aerol�neas? �Por qu� o por qu� no? (Sugerencia: piensa en vuelos %>% group_by(aerolinea, destino) %>% summarise(n()))


## 6 �Qu� hace el argumento sort a count(). �Cu�ndo podr�as usarlo?
##La funci�n count() nos permite saber cuantas observaciones hay en una variable especifica. Al agregar el argumento sort = TRUEdevuelve una tabla descendiente con el n�mero de observaciones.


# Parte 7: Dplyr - transformaciones agrupadas

## 1 Rem�tete a las listas de funciones �tiles de mutaci�n y filtrado. Describe c�mo cambia cada operaci�n cuando las combinas con la agrupaci�n.


## 2 �Qu� avi�n (codigo_cola) tiene el peor registro de tiempo?


## 3 �A qu� hora del d�a deber�as volar si quieres evitar lo m�s posible los retrasos?


## 4 Para cada destino, calcula los minutos totales de demora. Para cada vuelo, calcula la proporci�n de la demora total para su destino.


## 5 Los retrasos suelen estar temporalmente correlacionados: incluso una vez que el problema que caus� el retraso inicial se ha resuelto, los vuelos posteriores se retrasan para permitir que salgan los vuelos anteriores. Usando lag(), explora c�mo el retraso de un vuelo est� relacionado con el retraso del vuelo inmediatamente anterior.
 

## 6 Mira cada destino. �Puedes encontrar vuelos sospechosamente r�pidos? (es decir, vuelos que representan un posible error de entrada de datos). Calcula el tiempo en el aire de un vuelo relativo al vuelo m�s corto a ese destino. �Cu�les vuelos se retrasaron m�s en el aire?


## 7 Encuentra todos los destinos que son volados por al menos dos operadores. Usa esta informaci�n para clasificar a las aerol�neas.


## 8 Para cada avi�n, cuenta el n�mero de vuelos antes del primer retraso de m�s de 1 hora. 















                                             