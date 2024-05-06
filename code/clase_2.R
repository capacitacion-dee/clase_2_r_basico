

# Clase 2 -----------------------------------------------------------------
# Previo
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
vuelos %>% 
  count(aerolinea)

# Es lo mismo que...
count(vuelos, aerolinea)

# Trabajo básico con filas ------------------------------------------------

# Función filter
filter(vuelos, aerolinea=="UA")

# Usando %>% 
vuelos %>% 
  filter(aerolinea=="UA")

# Otros ejemplos
vuelos %>% 
  filter(mes>1)

vuelos %>% 
  filter(tiempo_vuelo == max(tiempo_vuelo, na.rm = TRUE))

vuelos %>% 
  filter(aerolinea!="AA" & mes == 12)

vuelos %>% 
  filter(atraso_llegada %in% 100:200)

# Operador %in%

vector1 <- c(1, 2, 3, 4, 5)
vector2 <- c(3, 4, 5, 6, 7)
vector1 %in% vector2


# Funciones slice 

# Extraer las cinco primeras filas de "vuelos"
slice_head(vuelos, n = 5)

# Usando %>% 
vuelos %>% 
  slice_head(n = 5)

# Extraer las filas 100 a la 105
slice(vuelos, 100:105)

# Usando %>% 
vuelos %>% 
  slice(100:105)

# Anidación de funciones --------------------------------------------------

# Usando filter y slice_head en conjunto
slice_head(filter(vuelos, aerolinea=="UA"), n = 5)

# Es lo mismo que...
vuelos %>% 
  filter(aerolinea=="UA") %>% 
  slice_head(n = 5)

# Utilizar la función count() para contar la frecuencia de ocurrencia de combinaciones únicas de los orígenes y destinos.
# Luego, utilizar la función filter() para seleccionar solo aquellas filas donde el valor de la columna 
# "origen" sea igual al aeropuerto "JFK".
# Finalmente, utilizar la función slice_head() para mostrar las primeras filas del resultado obtenido.
slice_head(filter(count(vuelos, origen, destino), origen=="JFK"), n=5)

# Es lo mismo que...
vuelos %>% 
  count(origen, destino) %>% 
  filter(origen == "JFK") %>% 
  slice_head(n = 5)


# Ejercicio 1 -------------------------------------------------------------

#- Del dataframe vuelos, obtenga las 100 primeras filas de vuelos
#- Filtre las filas donde los meses estén entre enero y marzo
#- Filtre las filas donde la aerolinea sea UA o DL
#- Filtre las filas donde la aerolinea sea UA o DL y agregue la condición de que el día sea igual a 12
#- Filtre la base donde el horario de salida este entre 500 y 1200, luego cuente el número de veces que aparece cada aerolinea





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
#- De la carpeta "data" cargue el excel "flores.xlsx" y llámelo "flores"
#- ¿Cuáles son los nombres de las columnas?
#- Seleccione las columnas Especie y Largo.Petalo
#- Seleccione las columnas que empiezan con "Largo"
#- Renombre "Especie" como "especie"




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
         distancia_sqrt = sqrt(distancia),
         origen_destino = paste(origen, destino, sep = "_"),
         atraso_salida_min = min(atraso_salida, na.rm = TRUE),
         atraso_llegada_max = max(atraso_llegada, na.rm = TRUE),
         aerolinea_minuscula = tolower(aerolinea)) %>% 
  select(distancia_promedio,distancia_sqrt,origen_destino,atraso_salida_min,atraso_llegada_max,aerolinea_minuscula)

## if_else ----

datos <- data.frame(nombre = c("Juan", "María", "Pedro", "Luis", "Ana"),
                    edad = c(15, 22, 30, 10, 25))

# Utilizando if_else para categorizar las edades
datos <- datos %>%
  mutate(categoria = if_else(edad < 18, "joven", "adulto"))

datos

# Generar una columna llamada tramo que sea "largo" si tiempo de vuelo fue mayor a 150, 
# en caso contrario sea "corto" y si es NA que sea "valor perdido". 
# Adicionalmente, queremos modificar la variable tiempo_vuelo":
# Si es NA (Valor perdido), reemplazar por un cero; de lo contrario, se mantiene el valor original.
vuelos %>% 
  mutate(tramo = if_else(tiempo_vuelo > 150, "largo", "corto", missing = "valor perdido"),
         tiempo_vuelo = if_else(is.na(tiempo_vuelo), 0, tiempo_vuelo))

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

# El orden importa
data <- data.frame(x = c(1, 2, 3, 4, 5))

data %>% mutate(y = case_when(x < 3 ~ "Bajo",
                              x < 5 ~ "Medio",
                              TRUE ~ "Alto"))

data %>% mutate(y = case_when(x < 5 ~ "Medio",
                              x < 3 ~ "Bajo",
                              TRUE ~ "Alto"))

# Otro ejemplo:
# crear una variable tramo donde si la distancia fue mayor a 3000 que sea "largo", 
# si fue menor a 3000 pero mayor o igual que 1000 que sea "mediano" y en cualquier otro caso que sea "corto".
vuelos <- vuelos %>% 
  mutate(tramo =  case_when(distancia >= 3000 ~ "largo",
                            distancia >= 2000 ~ "mediano",
                            TRUE ~ "corto"))
# Revisamos
vuelos %>% 
  select(distancia, tramo)

## group_by ----

estudiantes <- data.frame(
  nombre = c("Juan", "María", "Pedro", "Luis", "Ana"),
  edad = c(20, 22, 19, 21, 20),
  materia = c("Matemáticas", "Historia", "Matemáticas", "Historia", "Matemáticas")
)

estudiantes

# Utilizar group_by() para agrupar los datos por materia
estudiantes_por_materia <- estudiantes %>%
  group_by(materia)

estudiantes_por_materia

# Calcular el promedio de edad en cada materia
promedio_edad_por_materia <- estudiantes_por_materia %>%
  summarise(promedio_edad = mean(edad))

promedio_edad_por_materia

# Ejemplo usando vuelos

# Utilizar group_by() para agrupar los datos por aerolinea
vuelos_agrupado <- vuelos %>% 
  group_by(aerolinea) %>% 
  summarise(dist_promedio = mean(distancia, na.rm = TRUE))

vuelos_agrupado

# Ejercicio 3 -------------------------------------------------------------
# 1. Cargue el archivo excel “encuesta.xlsx” como “encuesta” contenido en la carpeta data.
# 2. Utilizando el operador %>% de forma anidada:
#   - Modifique la columna horas_tv: si horas tv es NA reemplazar por un 0; de lo contrario, 
#     se mantiene el valor original
#   - Genere una columna llamada tramo_edad que sea “Adulto mayor” cuando la edad sea mayor o igual que 50, 
#     “Adulto” cuando la edad esté entre 20 y 49 y “Joven” cuando la edad sea menor que 20.
#   - Agrupe los datos por “tramo_edad” y usando summarise() calcule las horas tv promedio para cada tramo, 
#     llamando a esta variable “horas_tv_prom”.




