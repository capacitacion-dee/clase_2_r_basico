

# CLase 2 -----------------------------------------------------------------

library(readxl)
library(dplyr)
vuelos <- read_excel("data/vuelos.xlsx")
vuelos

summary(vuelos)


# Operador pipe %>%  ------------------------------------------------------

# Obtener los seis primeros elementos de "vuelos" usando la función head()
vuelos %>% head()

# Es lo mismo que...
head(vuelos)

# Contar el número de veces que aparece cada aerolinea
vuelos %>% count(aerolinea)

# Es lo mismo que...
count(vuelos, aerolinea)

# Trabajo básico con filas ------------------------------------------------

# Función filter
filter(vuelos, aerolinea=="UA")

# Usando %>% 
vuelos %>% filter(aerolinea=="UA")

# Otros ejemplos
vuelos %>% filter(mes>1)
vuelos %>% filter(atraso_salida <= 100)
vuelos %>% filter(aerolinea!="AA" & mes == 12)
vuelos %>% filter(atraso_llegada %in% 100:200)


# Funciones slice 

# Extraer las cinco primeras filas de "vuelos"
slice_head(vuelos, n = 5)

# Usando %>% 
vuelos %>% slice_head(n = 5)

# Extraer las cinco últimas filas de "vuelos"
slice_tail(vuelos, n = 5)

# Usando %>% 
vuelos %>% slice_tail(n = 5)

# Obtener una muestra de filas aleatorias
slice_sample(vuelos, n = 5)

# Usando %>% 
vuelos %>% slice_sample(n = 5)

# Extraer las filas 100 a la 105
slice(vuelos, 100:105)

# Usando %>% 
vuelos %>% slice(100:105)

# Anidación de funciones --------------------------------------------------

# Usando filter y slice_head en conjunto
slice_head(filter(vuelos, aerolinea=="UA"), n = 5)

# Es lo mismo que...
vuelos %>% filter(aerolinea=="UA") %>% 
  slice_head(n = 5)

# Utilizar la función count() para contar la frecuencia de ocurrencia de combinaciones únicas de los orígenes y destinos.
# Luego, utilizar la función filter() para seleccionar solo aquellas filas donde el valor de la columna 
# "origen" sea igual al aeropuerto "JFK".
# Finalmente, utilizar la función slice_head() para mostrar las primeras filas del resultado obtenido.
slice_head(filter(count(vuelos, origen, destino), origen=="JFK"), n=5)

# Es lo mismo que...
vuelos %>% count(origen, destino) %>% filter(origen == "JFK") %>% slice_head(n = 5)


# Ejercicio 1 -------------------------------------------------------------

#- Obtenga las 100 primeras filas de vuelos
#- Del dataframe vuelos, filtre las filas donde los meses estén entre enero y marzo
#- Del dataframe vuelos filtre las filas donde la aerolinea sea UA o DL
#- Del dataframe vuelos filtre las filas donde la aerolinea sea UA o DL y agregue la condición de que el día sea igual a 12
#- Filtre la base donde el horario de salida este entre 500 y 1200, luego cuente el número de veces que aparece cada aerolinea
#- Utilice la misma anidación anterior pero ahora obtenga las cinco primeras filas





# Trabajo básico con columnas ---------------------------------------------

## Selección de columnas ----
# Por nombre
vuelos %>% 
  select(aerolinea, vuelo)

# Por posición
vuelos %>% 
  select(1,3,5)

# Por rango
vuelos %>% 
  select(1:4)

# Por selección negativa
vuelos %>% 
  select(-4,-6)

# Por condiciones en el nombre
vuelos %>% 
  select(starts_with("horario"))

vuelos %>% 
  select(ends_with("programada"))

vuelos %>% 
  select(contains("_"))

## Renombrar columnas ----
vuelos %>% 
  rename(salida = horario_salida,
         llegada = horario_llegada)


# Ejercicio 2 -------------------------------------------------------------
# - Cargue el excel "flores.xlsx" y llámelo flores
# - ¿Cuáles son los nombres de las columnas?
# - Seleccione las columnas Especie y Largo.Petalo
# - Seleccione las mismas columnas usando su posición
# - Seleccione las columnas que empiezan con "Largo"
# - Seleccione las columnas que terminen con "Sepalo"
# - Seleccione las columnas que no contengan "."




## Mutate ----

# Crear una columna que sea la suma del atraso en la salida y el atraso en la llegada llamada atraso_total
vuelos %>% 
  mutate(atraso_total = atraso_salida + atraso_llegada) %>% 
  select(atraso_total)

# Reemplazar una columna ya existente
vuelos %>% 
  mutate(distancia = distancia/2) %>% 
  select(distancia)

# Otros ejemplos
vuelos %>% 
  mutate(distancia_promedio = mean(distancia, na.rm = TRUE),
         distancia_cuadrado = sqrt(distancia),
         origen_destino = paste(origen, destino, sep = "_"),
         atraso_salida_min = min(atraso_salida, na.rm = TRUE),
         atraso_llegada_max = max(atraso_llegada, na.rm = TRUE),
         aerolinea_minuscula = tolower(aerolinea)) %>% 
  select(distancia_promedio,distancia_cuadrado,origen_destino,atraso_salida_min,atraso_llegada_max,aerolinea_minuscula)

## if_else ----

# Generar una columna llamada tramo que sea "largo" si tiempo  del vuelo fue mayor a 150, 
# en caso contrario sea "corto" y si es NA que sea "valor perdido". 
# Adicionalmente, podríamos querer modificar la variable tiempo_vuelo. 
# Si es NA (Valor perdido), reemplazar por un cero; de lo contrario, se mantiene el valor original.
vuelos %>% 
  mutate(tramo = if_else(condition = tiempo_vuelo > 150, true = "largo", false = "corto", missing = "valor perdido"),
         tiempo_vuelo = if_else(is.na(tiempo_vuelo), 0, tiempo_vuelo))

# Generaremos una variable del tipo fecha que sea del siguiente formato: "2013-02-12"
vuelos %>% select(dia, mes) %>% head()

# si el largo de la variable mes o día es menor que uno, le pegaremos un cero al comienzo, en caso contrario, 
# dejaremos el mes o el dia como están orginalmente pero convertiéndolos a formato character
vuelos2 <- vuelos %>% 
  mutate(dia2 = if_else(nchar(dia) == 1, paste0("0", dia), as.character(dia)),
         mes2 = if_else(nchar(mes) == 1, paste0("0", mes), as.character(mes)))

vuelos2 %>% select(dia2, mes2) %>% head()

# Luego pegamos el año, el mes y el día usando como separador "-"
vuelos2 <- vuelos2 %>% 
  mutate(fecha = paste(anio, mes2, dia2, sep = "-"))

vuelos2 %>% select(fecha) %>% head()

# Finalmente, convertimos la variable fecha que está en formato "character" a formato fecha usando la función as.Date()
vuelos2 <- vuelos2 %>% 
  mutate(fecha = as.Date(fecha))

vuelos2 %>% select(fecha) %>% head()

## case_when ----

datos <- data.frame(puntuacion = c(80, 65, 90, 75, 50))
datos <- datos %>%
  mutate(resultado = case_when(
    puntuacion >= 90 ~ "A",
    puntuacion >= 80 ~ "B",
    puntuacion >= 70 ~ "C",
    TRUE ~ "F" # En caso de que ninguna de las condiciones anteriores se cumpla
  ))
datos


# crear una variable tramo donde si la distancia fue mayor a 3000 que sea "largo", 
# si fue menor a 3000 pero mayor o igual que 1000 que sea "mediano" y en cualquier otro caso que sea "corto".
vuelos <- vuelos %>% 
  mutate(tramo =  case_when(distancia >= 3000 ~ "largo",
                            distancia >= 2000 ~ "mediano",
                            distancia >= 1000 ~ "corto",
                            TRUE ~ "muy corto"))
# Revisamos
vuelos %>% select(distancia, tramo) %>% 
  slice_head(n = 20)

