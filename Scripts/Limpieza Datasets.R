require(tidyverse)

#### Limpieza del dataset trips_2022_reducido.csv  ###

df_bici = read_csv('Data/trips_2022_reducido.csv') 
df_bici = df_bici %>%
  filter( 
    (duracion_recorrido >= 300) &
      (duracion_recorrido <= 3600)
  ) %>%            # Filtramos los recorridos cuya duracion sea de entre 5 minutos y una hora 
  mutate(
    # Género = if_else(is.na(Género), "OTHER", Género),  # Si el genero es NA lo reemplazamos por otro
    nombre_estacion_origen = gsub('\\.','. ',nombre_estacion_origen),
    nombre_estacion_destino = gsub('\\.','. ',nombre_estacion_destino),
    nombre_estacion_origen = str_to_title(nombre_estacion_origen),
    nombre_estacion_destino = str_to_title(nombre_estacion_destino)
  ) %>%
  select(!c(X.1, X, id_estacion_origen, direccion_estacion_origen, id_estacion_destino, direccion_estacion_destino))

# Normalizamos los nombres delas columnas
# vemos los indices para renombrar las columnas
colnames(df_bici)

colnames(df_bici)[1] = 'id_recorrido'   
colnames(df_bici)[13] = 'genero'
colnames(df_bici)

# Guardamos el dataset procesado 

write.csv(df_bici, file='Data/Bicis_limpio.csv', row.names = F)

View(read_csv('Data/Bicis_limpio.csv'))





#### Limpieza del dataset clima.csv  ###

df_clima = read_csv('Data/clima.csv') 

# Hay dos columnas que parecieran ser todo NA. Lo confirmamos

distinct_snow <- df_clima %>% distinct(snow)
view(distinct_snow)  # Devuelve todo NA

distinct_wgpt <- df_clima %>% distinct(wpgt)
view(distinct_wgpt)  # Devuelve todo NA

distinct_tsun <- df_clima %>% distinct(tsun)
view(distinct_tsun)  # Devuelve todo NA


# Eliminamos esas dos columnas porque ambas tienen unicamente el valor NA

df_clima <- df_clima %>% select(-snow, -wpgt, -tsun)

view(df_clima)


# Ahora queremos renombras las columnas para tener valores mas sencillos

nuevos_nombres <- c("fecha", "temp_prom", "temp_min", "temp_max", "lluvias_mm", "viento_dir", "viento_veloc")

df_clima <- df_clima %>%
  rename(
    fecha = date,
    temp_prom = tavg,
    temp_min = tmin,
    temp_max = tmax,
    lluvias_mm = prcp,
    viento_dir = wdir,
    viento_veloc = wspd,
    presion = pres
  )

# Por ultimo queremos ver si queda algun valor NA en el dataset

valores_na_por_columna <- sapply(df_clima, function(x) sum(is.na(x)))

# Visualizamos la cantidad de valores NA por columna
print(valores_na_por_columna) # La unica columna donde hallamos valores NA es en lluvias_mm donde hay 2 valores NA

view(df_clima)

# Reemplacemos esos 2 NA por el promedio de mm del año



# prom_lluvias <- round(mean(df_clima$lluvias_mm, na.rm = TRUE), 2)
# 
# # Reemplazamos los valores NA en la columna 'lluvias_mm' con el promedio
# df_clima$lluvias_mm[is.na(df_clima$lluvias_mm)] <- prom_lluvias
# 
# # Visualizamos nuevamente para ver que no queden NA en el dataset
# valores_na_por_columna <- sapply(df_clima, function(x) sum(is.na(x)))
# print(valores_na_por_columna)

# No quedan valores NA.
# Guardamos el dataset procesado 

write.csv(df_clima, file='Data/Clima_limpio.csv', row.names = F)

View(read_csv('Data/Clima_limpio.csv'))



