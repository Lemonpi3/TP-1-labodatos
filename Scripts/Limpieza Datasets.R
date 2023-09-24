require(tidyverse)

#Bicis
df_bici = read_csv('Data/trips_2022_reducido.csv') 
df_bici = df_bici %>%
  filter( 
    (duracion_recorrido >= 300) &
      (duracion_recorrido <= 3600)
  ) %>%
  mutate(
    Género = if_else(is.na(Género), "OTHER", Género),
    nombre_estacion_origen = gsub('\\.','. ',nombre_estacion_origen),
    nombre_estacion_destino = gsub('\\.','. ',nombre_estacion_destino),
    nombre_estacion_origen = str_to_title(nombre_estacion_origen),
    nombre_estacion_destino = str_to_title(nombre_estacion_destino)
  ) %>%
  select(!c(X.1, X, id_estacion_origen, direccion_estacion_origen, id_estacion_destino, direccion_estacion_destino))

#Normalizo los nombres delas columnas
#veo los inices
colnames(df_bici)

colnames(df_bici)[1] = 'id_recorrido'
colnames(df_bici)[13] = 'genero'
colnames(df_bici)

write.csv(df_bici, file='Data/Bicis_limpio.csv', row.names = F)

View(read_csv('Data/Bicis_limpio.csv'))
