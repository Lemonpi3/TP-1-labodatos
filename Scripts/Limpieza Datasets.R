require(tidyverse)

#Bicis
df_bici = read_csv('Data/trips_2022_reducido.csv') 
df_bici = df_bici %>%
  filter( 
    (duracion_recorrido >= 300) &
      (duracion_recorrido <= 3600)
  ) %>%
  mutate(
    Género = if_else(is.na(Género), "OTHER", Género)
  ) %>%
  select(!c(X.1, X, id_estacion_origen, direccion_estacion_origen, id_estacion_destino, direccion_estacion_destino))

write.csv(df_bici, file='Data/Bicis_limpio.csv', row.names = F)

View(read_csv('Data/Bicis_limpio.csv'))