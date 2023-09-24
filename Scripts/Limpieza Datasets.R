# Obtenemos la ruta del directorio del script actual
ruta_script <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Construimos las rutas a los archivos y directorios relativos
ruta_data <- file.path(ruta_script, '..', 'Data')
ruta_entrada <- file.path(ruta_data, 'trips_2022_reducido.csv')
ruta_salida <- file.path(ruta_data, 'Bicis_limpio.csv')

# Cargamos el archivo de entrada
df_bici <- read_csv(ruta_entrada)

# Procesamos los datos

# Filtramos los datos para que cumplan con ciertas condiciones en la duración del recorrido
df_bici <- df_bici %>%
  filter(duracion_recorrido >= 300 & duracion_recorrido <= 3600)

# Realizamos transformaciones en las columnas
df_bici <- df_bici %>%
  mutate(
    # Si el género está ausente, lo reemplazamos con "OTHER"
    Género = if_else(is.na(Género), "OTHER", Género),
    
    # Normalizamos y modificamos el formato de los nombres de estación
    nombre_estacion_origen = gsub('\\.','. ',nombre_estacion_origen),
    nombre_estacion_destino = gsub('\\.','. ',nombre_estacion_destino),
    nombre_estacion_origen = str_to_title(nombre_estacion_origen),
    nombre_estacion_destino = str_to_title(nombre_estacion_destino)
  )

# Seleccionamos columnas específicas para mantener
df_bici <- df_bici %>%
  select(-X.1, -X, -id_estacion_origen, -direccion_estacion_origen, -id_estacion_destino, -direccion_estacion_destino)

# Cambiamos al directorio donde se guardará el archivo de salida
setwd(ruta_data)

# Guardamos el archivo procesado
write.csv(df_bici, file = ruta_salida, row.names = FALSE)

# Visualizamos el archivo procesado
View(read_csv(ruta_salida))
