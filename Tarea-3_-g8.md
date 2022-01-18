EJERCICIOS_G18
================
Silva C.,Angela Ximena, Vicharra C.,Carmen & Zelada H., Claudia
13/1/2022

# Pasos iniciales

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.1.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 4.1.2

``` r
nycflights13::flights
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
flights
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
View(flights)
```

# Parte 1: Dplyr - filter

## 1 Encuentra todos los vuelos que

### A) Tubieron un retrazo de llegada de dos o más horas

``` r
v_retraso <- filter(flights, arr_delay >= 120 )
View(v_retraso)
v_retraso
```

    ## # A tibble: 10,200 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      811            630       101     1047            830
    ##  2  2013     1     1      848           1835       853     1001           1950
    ##  3  2013     1     1      957            733       144     1056            853
    ##  4  2013     1     1     1114            900       134     1447           1222
    ##  5  2013     1     1     1505           1310       115     1638           1431
    ##  6  2013     1     1     1525           1340       105     1831           1626
    ##  7  2013     1     1     1549           1445        64     1912           1656
    ##  8  2013     1     1     1558           1359       119     1718           1515
    ##  9  2013     1     1     1732           1630        62     2028           1825
    ## 10  2013     1     1     1803           1620       103     2008           1750
    ## # ... with 10,190 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### B) Volaron a Houston (IAH oHOU)

``` r
filter(flights, dest == "IAH" | dest == "HOU")
```

    ## # A tibble: 9,313 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      623            627        -4      933            932
    ##  4  2013     1     1      728            732        -4     1041           1038
    ##  5  2013     1     1      739            739         0     1104           1038
    ##  6  2013     1     1      908            908         0     1228           1219
    ##  7  2013     1     1     1028           1026         2     1350           1339
    ##  8  2013     1     1     1044           1045        -1     1352           1351
    ##  9  2013     1     1     1114            900       134     1447           1222
    ## 10  2013     1     1     1205           1200         5     1503           1505
    ## # ... with 9,303 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### C) Fueron operados por United, American o Delta

``` r
filter(flights, carrier == "USA" | carrier == "AA" | carrier == "DL")
```

    ## # A tibble: 80,839 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      542            540         2      923            850
    ##  2  2013     1     1      554            600        -6      812            837
    ##  3  2013     1     1      558            600        -2      753            745
    ##  4  2013     1     1      559            600        -1      941            910
    ##  5  2013     1     1      602            610        -8      812            820
    ##  6  2013     1     1      606            610        -4      858            910
    ##  7  2013     1     1      606            610        -4      837            845
    ##  8  2013     1     1      615            615         0      833            842
    ##  9  2013     1     1      623            610        13      920            915
    ## 10  2013     1     1      628            630        -2     1137           1140
    ## # ... with 80,829 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### D) Partieron en invierno del hemisferio sur (julio, agosto y septiembre)

``` r
filter(flights, month == 7 |  month == 8 | month == 9)
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7     1        1           2029       212      236           2359
    ##  2  2013     7     1        2           2359         3      344            344
    ##  3  2013     7     1       29           2245       104      151              1
    ##  4  2013     7     1       43           2130       193      322             14
    ##  5  2013     7     1       44           2150       174      300            100
    ##  6  2013     7     1       46           2051       235      304           2358
    ##  7  2013     7     1       48           2001       287      308           2305
    ##  8  2013     7     1       58           2155       183      335             43
    ##  9  2013     7     1      100           2146       194      327             30
    ## 10  2013     7     1      100           2245       135      337            135
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### E) Llegaron más de dos horas tarde, pero no salieron tarde

``` r
filter(flights, arr_delay > 120, dep_delay <=0)
```

    ## # A tibble: 29 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1    27     1419           1420        -1     1754           1550
    ##  2  2013    10     7     1350           1350         0     1736           1526
    ##  3  2013    10     7     1357           1359        -2     1858           1654
    ##  4  2013    10    16      657            700        -3     1258           1056
    ##  5  2013    11     1      658            700        -2     1329           1015
    ##  6  2013     3    18     1844           1847        -3       39           2219
    ##  7  2013     4    17     1635           1640        -5     2049           1845
    ##  8  2013     4    18      558            600        -2     1149            850
    ##  9  2013     4    18      655            700        -5     1213            950
    ## 10  2013     5    22     1827           1830        -3     2217           2010
    ## # ... with 19 more rows, and 11 more variables: arr_delay <dbl>, carrier <chr>,
    ## #   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
    ## #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### F) Se retrasaron por lo menos una hora, pero repusieron más de 30 minutos en vuelo

``` r
filter(flights, dep_delay >= 60, arr_delay < 30)
```

    ## # A tibble: 206 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     3     1850           1745        65     2148           2120
    ##  2  2013     1     3     1950           1845        65     2228           2227
    ##  3  2013     1     3     2015           1915        60     2135           2111
    ##  4  2013     1     6     1019            900        79     1558           1530
    ##  5  2013     1     7     1543           1430        73     1758           1735
    ##  6  2013     1    11     1020            920        60     1311           1245
    ##  7  2013     1    12     1706           1600        66     1949           1927
    ##  8  2013     1    12     1953           1845        68     2154           2137
    ##  9  2013     1    19     1456           1355        61     1636           1615
    ## 10  2013     1    21     1531           1430        61     1843           1815
    ## # ... with 196 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### G) Partieron entre la medianoche y las 6 a.m. (incluyente)

``` r
filter(flights, dep_time >= 0 & dep_time <= 600)
```

    ## # A tibble: 9,344 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 9,334 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

## 2 Otra función de dplyr que es útil para usar filtros es between(). ¿Qué hace? ¿Puedes usarla para simplificar el código necesario para responder a los desafíos anteriores?

``` r
# Nos Devuelve un valor lógico que indica si el valor especificado está dentro de un rango.

# Para la pregunta D: vuelos que partieron en invierno del hemisferio sur (julio, agosto, septiembre)


filter(flights, between(month, 7,9))
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7     1        1           2029       212      236           2359
    ##  2  2013     7     1        2           2359         3      344            344
    ##  3  2013     7     1       29           2245       104      151              1
    ##  4  2013     7     1       43           2130       193      322             14
    ##  5  2013     7     1       44           2150       174      300            100
    ##  6  2013     7     1       46           2051       235      304           2358
    ##  7  2013     7     1       48           2001       287      308           2305
    ##  8  2013     7     1       58           2155       183      335             43
    ##  9  2013     7     1      100           2146       194      327             30
    ## 10  2013     7     1      100           2245       135      337            135
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

## 3 ¿Cuántos vuelos tienen datos faltantes en horario_salida? ¿Qué otras variables tienen valores faltantes? ¿Qué representan estas filas?

``` r
# ¿Cuántos vuelos tienen datos faltantes en horario_salida?

View(count((filter(flights, is.na(dep_time)))))

# ¿Qué otras variables tienen valores faltantes?

View(filter(flights, is.na(dep_time)))
## dep_delay, arr_time, arr_delay, tailnum, air_time 


# ¿Qué representan estas filas?
 ## Las filas con datos faltantes(NA) representan los vuelos que fueron cancelados. Ya que al ser cancelado, se entiende que no hay datos de de retraso en salida o en llegada del vuelo. 
```

## 4 ¿Por qué NA^0 no es faltante? ¿Por qué NA \| TRUE no es faltante? ¿Por qué FALSE & NA no es faltante? ¿Puedes descubrir la regla general? (¡NA \* 0 es un contraejemplo complicado!)

``` r
# ¿Por qué NA^0 no es faltante?
x <- c(NA) 
is.na(x)
```

    ## [1] TRUE

``` r
#Dado que el NA podría tomar cualquier valor, es práctico pensar que cualquier número (aunque sea muy grande) a la potencia cero es igual a 1.

# ¿Por qué NA | TRUE no es faltante?
#Es igual a TRUE pues el NA se entiende como un valor lógico (`TRUE` or `FALSE`) y por lógica proposicional `TRUE` | `TRUE`  y  `FALSE` | `TRUE` es siempre igual a `TRUE`.
x | TRUE
```

    ## [1] TRUE

``` r
# ¿Por qué FALSE & NA no es faltante?
x & FALSE
```

    ## [1] FALSE

``` r
# Es igual a `TRUE` pues el NA se entiende como un valor lógico (`TRUE` or `FALSE`) y por lógica proposicional `TRUE`&`FALSE` y `FALSE`&`FALSE` es siempre `FALSE`.
```

# Parte 2: Dplyr - arrange

## 1 ¿Cómo podrías usar arrange() para ordenar todos los valores faltantes al comienzo? (Sugerencia: usa is.na()).

``` r
arrange(flights,is.na(air_time))
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

## 2 Ordena vuelos para encontrar los vuelos más retrasados. Encuentra los vuelos que salieron más temprano.

``` r
View(arrange(flights, desc(dep_delay)))
View(arrange(flights, dep_delay > 0))
```

## 3 Ordena vuelos para encontrar los vuelos más rápidos (que viajaron a mayor velocidad).

``` r
vuelos02 <- arrange( flights, desc(distance / air_time))
head(vuelos02)
```

    ## # A tibble: 6 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     5    25     1709           1700         9     1923           1937
    ## 2  2013     7     2     1558           1513        45     1745           1719
    ## 3  2013     5    13     2040           2025        15     2225           2226
    ## 4  2013     3    23     1914           1910         4     2045           2043
    ## 5  2013     1    12     1559           1600        -1     1849           1917
    ## 6  2013    11    17      650            655        -5     1059           1150
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

## 4 ¿Cuáles vuelos viajaron más lejos? ¿Cuál viajó más cerca?

``` r
## ¿Cuáles vuelos viajaron más lejos?

mas_lejos <- arrange(flights, desc(distance))
mas_lejos
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      857            900        -3     1516           1530
    ##  2  2013     1     2      909            900         9     1525           1530
    ##  3  2013     1     3      914            900        14     1504           1530
    ##  4  2013     1     4      900            900         0     1516           1530
    ##  5  2013     1     5      858            900        -2     1519           1530
    ##  6  2013     1     6     1019            900        79     1558           1530
    ##  7  2013     1     7     1042            900       102     1620           1530
    ##  8  2013     1     8      901            900         1     1504           1530
    ##  9  2013     1     9      641            900      1301     1242           1530
    ## 10  2013     1    10      859            900        -1     1449           1530
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
## ¿Cuál viajó más cerca?

mas_cerca <- arrange(flights, distance)
mas_cerca
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7    27       NA            106        NA       NA            245
    ##  2  2013     1     3     2127           2129        -2     2222           2224
    ##  3  2013     1     4     1240           1200        40     1333           1306
    ##  4  2013     1     4     1829           1615       134     1937           1721
    ##  5  2013     1     4     2128           2129        -1     2218           2224
    ##  6  2013     1     5     1155           1200        -5     1241           1306
    ##  7  2013     1     6     2125           2129        -4     2224           2224
    ##  8  2013     1     7     2124           2129        -5     2212           2224
    ##  9  2013     1     8     2127           2130        -3     2304           2225
    ## 10  2013     1     9     2126           2129        -3     2217           2224
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

# Parte 3: Dplyr - select

## 1 Haz una lluvia de ideas sobre tantas maneras como sea posible para seleccionar dep_time, dep_delay, arr_time, and arr_delay de flights.

``` r
select(flights, dep_time, dep_delay, arr_time, arr_delay)
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_delay arr_time arr_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      517         2      830        11
    ##  2      533         4      850        20
    ##  3      542         2      923        33
    ##  4      544        -1     1004       -18
    ##  5      554        -6      812       -25
    ##  6      554        -4      740        12
    ##  7      555        -5      913        19
    ##  8      557        -3      709       -14
    ##  9      557        -3      838        -8
    ## 10      558        -2      753         8
    ## # ... with 336,766 more rows

``` r
select(flights, starts_with("carrier"), starts_with("time"))
```

    ## # A tibble: 336,776 x 2
    ##    carrier time_hour          
    ##    <chr>   <dttm>             
    ##  1 UA      2013-01-01 05:00:00
    ##  2 UA      2013-01-01 05:00:00
    ##  3 AA      2013-01-01 05:00:00
    ##  4 B6      2013-01-01 05:00:00
    ##  5 DL      2013-01-01 06:00:00
    ##  6 UA      2013-01-01 05:00:00
    ##  7 B6      2013-01-01 06:00:00
    ##  8 EV      2013-01-01 06:00:00
    ##  9 B6      2013-01-01 06:00:00
    ## 10 AA      2013-01-01 06:00:00
    ## # ... with 336,766 more rows

## 2 ¿Qué sucede si incluyes el nombre de una variable varias veces en una llamada a select()?

``` r
select(flights, dep_delay, arr_time, dep_delay, carrier, carrier)
```

    ## # A tibble: 336,776 x 3
    ##    dep_delay arr_time carrier
    ##        <dbl>    <int> <chr>  
    ##  1         2      830 UA     
    ##  2         4      850 UA     
    ##  3         2      923 AA     
    ##  4        -1     1004 B6     
    ##  5        -6      812 DL     
    ##  6        -4      740 UA     
    ##  7        -5      913 B6     
    ##  8        -3      709 EV     
    ##  9        -3      838 B6     
    ## 10        -2      753 AA     
    ## # ... with 336,766 more rows

## 3 ¿Qué hace la función any_of()? ¡¿Por qué podría ser útil en conjunto con este vector?

``` r
# ¿Qué hace la función any_of()?

### La función any_of() al igual que la función all_of()coinciden con nombres de variables en un vector de caracteres, en all_of()todos los nombres deben de estar presentes de lo contrario , se genera un error de fuera de límites ; en cambio any_of() no arroja ningún error para los nombres que no existen.

# ¿Por qué podría ser útil en conjunto con este vector?

### La función any_of() ayuda a dbplyr a que se implemente un dialecto de R donde los operadores facilitan la selección de variables , en este caso por medio del vector ; estos ayudantes seleccionan variables de un vector de caracteres.
```

# Parte 4: Dplyr - mutate

## 1 Las variables horario_salida y salida_programada tienen un formato conveniente para leer, pero es difícil realizar cualquier cálculo con ellas porque no son realmente números continuos. Transfórmalas hacia un formato más conveniente como número de minutos desde la medianoche.

``` r
# dep_time

vuelosABC <- select(flights,
                    year:month,
                    starts_with("dep_time"))
mutate(vuelosABC,
       hora = dep_time %/% 100,
       minuto = dep_time %% 100)              
```

    ## # A tibble: 336,776 x 5
    ##     year month dep_time  hora minuto
    ##    <int> <int>    <int> <dbl>  <dbl>
    ##  1  2013     1      517     5     17
    ##  2  2013     1      533     5     33
    ##  3  2013     1      542     5     42
    ##  4  2013     1      544     5     44
    ##  5  2013     1      554     5     54
    ##  6  2013     1      554     5     54
    ##  7  2013     1      555     5     55
    ##  8  2013     1      557     5     57
    ##  9  2013     1      557     5     57
    ## 10  2013     1      558     5     58
    ## # ... with 336,766 more rows

``` r
transmute(vuelosABC,
          hora = dep_time %/% 100,
          minuto = dep_time %% 100)
```

    ## # A tibble: 336,776 x 2
    ##     hora minuto
    ##    <dbl>  <dbl>
    ##  1     5     17
    ##  2     5     33
    ##  3     5     42
    ##  4     5     44
    ##  5     5     54
    ##  6     5     54
    ##  7     5     55
    ##  8     5     57
    ##  9     5     57
    ## 10     5     58
    ## # ... with 336,766 more rows

``` r
# sched_dep_time

vuelosABC <- select(flights,
                    year:month,
                    starts_with("sched_dep_time"))
mutate(vuelosABC,
       hora = sched_dep_time %/% 100,
       minuto = sched_dep_time %% 100)              
```

    ## # A tibble: 336,776 x 5
    ##     year month sched_dep_time  hora minuto
    ##    <int> <int>          <int> <dbl>  <dbl>
    ##  1  2013     1            515     5     15
    ##  2  2013     1            529     5     29
    ##  3  2013     1            540     5     40
    ##  4  2013     1            545     5     45
    ##  5  2013     1            600     6      0
    ##  6  2013     1            558     5     58
    ##  7  2013     1            600     6      0
    ##  8  2013     1            600     6      0
    ##  9  2013     1            600     6      0
    ## 10  2013     1            600     6      0
    ## # ... with 336,766 more rows

``` r
transmute(vuelosABC,
          hora = sched_dep_time %/% 100,
          minuto = sched_dep_time %% 100)
```

    ## # A tibble: 336,776 x 2
    ##     hora minuto
    ##    <dbl>  <dbl>
    ##  1     5     15
    ##  2     5     29
    ##  3     5     40
    ##  4     5     45
    ##  5     6      0
    ##  6     5     58
    ##  7     6      0
    ##  8     6      0
    ##  9     6      0
    ## 10     6      0
    ## # ... with 336,766 more rows

## 2 Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué esperas ver? ¿Qué ves? ¿Qué necesitas hacer para arreglarlo?

``` r
vuelos01 <- select(flights, dep_time, arr_time)

mutate(flights, 
       tiempo_vuelo = arr_time - dep_time)
```

    ## # A tibble: 336,776 x 20
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   tiempo_vuelo <int>

## 3 Compara horario_salida, salida_programada, y atraso_salida. ¿Cómo esperarías que esos tres números estén relacionados?

``` r
#Podemos indicar que el atraso_salida = horario_salida - salida_programada

select(flights, dep_time, sched_dep_time, dep_delay)
```

    ## # A tibble: 336,776 x 3
    ##    dep_time sched_dep_time dep_delay
    ##       <int>          <int>     <dbl>
    ##  1      517            515         2
    ##  2      533            529         4
    ##  3      542            540         2
    ##  4      544            545        -1
    ##  5      554            600        -6
    ##  6      554            558        -4
    ##  7      555            600        -5
    ##  8      557            600        -3
    ##  9      557            600        -3
    ## 10      558            600        -2
    ## # ... with 336,766 more rows

``` r
transmute(flights, tiempo_vuel0 = dep_time - sched_dep_time)
```

    ## # A tibble: 336,776 x 1
    ##    tiempo_vuel0
    ##           <int>
    ##  1            2
    ##  2            4
    ##  3            2
    ##  4           -1
    ##  5          -46
    ##  6           -4
    ##  7          -45
    ##  8          -43
    ##  9          -43
    ## 10          -42
    ## # ... with 336,766 more rows

## 4 Encuentra los 10 vuelos más retrasados utilizando una función de ordenamiento. ¿Cómo quieres manejar los empates? Lee atentamente la documentación de min_rank().

``` r
vuelos_retraso <-head(arrange(flights,desc(arr_delay)),10)
vuelos_retraso
```

    ## # A tibble: 10 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     9      641            900      1301     1242           1530
    ##  2  2013     6    15     1432           1935      1137     1607           2120
    ##  3  2013     1    10     1121           1635      1126     1239           1810
    ##  4  2013     9    20     1139           1845      1014     1457           2210
    ##  5  2013     7    22      845           1600      1005     1044           1815
    ##  6  2013     4    10     1100           1900       960     1342           2211
    ##  7  2013     3    17     2321            810       911      135           1020
    ##  8  2013     7    22     2257            759       898      121           1026
    ##  9  2013    12     5      756           1700       896     1058           2020
    ## 10  2013     5     3     1133           2055       878     1250           2215
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
vuelos_10<-vuelos_retraso$arr_delay
vuelos_10
```

    ##  [1] 1272 1127 1109 1007  989  931  915  895  878  875

``` r
min_rank(vuelos_10)
```

    ##  [1] 10  9  8  7  6  5  4  3  2  1

## 5 ¿Qué devuelve 1:3 + 1:10? ¿Por qué?

``` r
1:3 + 1:10
```

    ## Warning in 1:3 + 1:10: longitud de objeto mayor no es múltiplo de la longitud de
    ## uno menor

    ##  [1]  2  4  6  5  7  9  8 10 12 11

``` r
#Nos devuelve que la longitud del objeto más largo(1:10) no es un múltiplo de la longitud del objeto más corto(1:3)
```

## 6 ¿Qué funciones trigonométricas proporciona R?

``` r
# R proporciona las funciones coseno, tangente y cotangente

sin(pi/3)
```

    ## [1] 0.8660254

``` r
cos(pi)
```

    ## [1] -1

``` r
tan(pi/2)
```

    ## [1] 1.633124e+16

``` r
1/tan(pi)
```

    ## [1] -8.16562e+15

# Parte 5\_ Dplyr - group by & summarize

## 1 Haz una lluvia de ideas de al menos 5 formas diferentes de evaluar las características de un retraso típico de un grupo de vuelos. Considera los siguientes escenarios:

### A) Un vuelo llega 15 minutos antes 50% del tiempo, y 15 minutos tarde 50% del tiempo.

### B) Un vuelo llega siempre 10 minutos tarde.

### C) Un vuelo llega 30 minutos antes 50% del tiempo, y 30 minutos tarde 50% del tiempo.

### D) Un vuelo llega a tiempo en el 99% de los casos. 1% de las veces llega 2 horas tarde. ¿Qué es más importante: retraso de la llegada o demora de salida?

## 2 Sugiere un nuevo enfoque que te dé el mismo output que no_cancelados %>% count(destino) y no_cancelado %>% count(codigo_cola, wt = distancia) (sin usar count()).

## 3 Nuestra definición de vuelos cancelados (is.na(atraso_salida) \| is.na (atraso_llegada)) es un poco subóptima. ¿Por qué? ¿Cuál es la columna más importante?

``` r
#por definición
no_cancelados <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
no_cancelados
```

    ## # A tibble: 327,346 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 327,336 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
#otra forma
no_cancelados %>% 
  group_by(year, month, day) %>% 
  summarise(media = mean(dep_delay))
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.

    ## # A tibble: 365 x 4
    ## # Groups:   year, month [12]
    ##     year month   day media
    ##    <int> <int> <int> <dbl>
    ##  1  2013     1     1 11.4 
    ##  2  2013     1     2 13.7 
    ##  3  2013     1     3 10.9 
    ##  4  2013     1     4  8.97
    ##  5  2013     1     5  5.73
    ##  6  2013     1     6  7.15
    ##  7  2013     1     7  5.42
    ##  8  2013     1     8  2.56
    ##  9  2013     1     9  2.30
    ## 10  2013     1    10  2.84
    ## # ... with 355 more rows

``` r
#La columna mas importante es el promedio de vuelos no cancelados
```

## 4 Mira la cantidad de vuelos cancelados por día. ¿Hay un patrón? ¿La proporción de vuelos cancelados está relacionada con el retraso promedio?

``` r
por_dia <- group_by(flights, year, month, day)
summarise(por_dia, retraso = mean(dep_delay, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.

    ## # A tibble: 365 x 4
    ## # Groups:   year, month [12]
    ##     year month   day retraso
    ##    <int> <int> <int>   <dbl>
    ##  1  2013     1     1   11.5 
    ##  2  2013     1     2   13.9 
    ##  3  2013     1     3   11.0 
    ##  4  2013     1     4    8.95
    ##  5  2013     1     5    5.73
    ##  6  2013     1     6    7.15
    ##  7  2013     1     7    5.42
    ##  8  2013     1     8    2.55
    ##  9  2013     1     9    2.28
    ## 10  2013     1    10    2.84
    ## # ... with 355 more rows

## 5 ¿Qué compañía tiene los peores retrasos? Desafío: ¿puedes desenredar el efecto de malos aeropuertos vs. el efecto de malas aerolíneas? ¿Por qué o por qué no? (Sugerencia: piensa en vuelos %>% group_by(aerolinea, destino) %>% summarise(n()))

## 6 ¿Qué hace el argumento sort a count(). ¿Cuándo podrías usarlo?

``` r
##La función count() nos permite saber cuantas observaciones hay en una variable especifica. Al agregar el argumento sort = TRUEdevuelve una tabla descendiente con el número de observaciones.
```

# Parte 7: Dplyr - transformaciones agrupadas

## 1 Remítete a las listas de funciones útiles de mutación y filtrado. Describe cómo cambia cada operación cuando las combinas con la agrupación.

## 2 ¿Qué avión (codigo_cola) tiene el peor registro de tiempo?

## 3 ¿A qué hora del día deberías volar si quieres evitar lo más posible los retrasos?

## 4 Para cada destino, calcula los minutos totales de demora. Para cada vuelo, calcula la proporción de la demora total para su destino.

## 5 Los retrasos suelen estar temporalmente correlacionados: incluso una vez que el problema que causó el retraso inicial se ha resuelto, los vuelos posteriores se retrasan para permitir que salgan los vuelos anteriores. Usando lag(), explora cómo el retraso de un vuelo está relacionado con el retraso del vuelo inmediatamente anterior.

## 6 Mira cada destino. ¿Puedes encontrar vuelos sospechosamente rápidos? (es decir, vuelos que representan un posible error de entrada de datos). Calcula el tiempo en el aire de un vuelo relativo al vuelo más corto a ese destino. ¿Cuáles vuelos se retrasaron más en el aire?

## 7 Encuentra todos los destinos que son volados por al menos dos operadores. Usa esta información para clasificar a las aerolíneas.

## 8 Para cada avión, cuenta el número de vuelos antes del primer retraso de más de 1 hora.
