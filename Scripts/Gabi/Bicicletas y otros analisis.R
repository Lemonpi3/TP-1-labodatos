df_bici = read_csv('Data/Bicis_limpio.csv')
df_clima = read_csv('Data/Clima_limpio.csv')

view(df_bici)

# names(df_bici) =  names(df_bici)
# "id_recorrido"            "duracion_recorrido"
# "fecha_origen_recorrido"  "nombre_estacion_origen"
# "long_estacion_origen"    "lat_estacion_origen"
# "fecha_destino_recorrido" "nombre_estacion_destino"
# "long_estacion_destino"   "lat_estacion_destino"
# "id_usuario"              "modelo_bicicleta"
# "genero"                  "fecha"
# 
# 
# names(df_clima) = "fecha"        "temp_prom"    "temp_min"     "temp_max"
# "lluvias_mm"   "viento_dir"   "viento_veloc" "presion"


library(ggplot2)
library(dplyr)

################## MODELOS DE BICI MAS USADOS #############


# Left join de df_bici con df_clima por la columna "fecha"
df_joineado <- left_join(df_bici, df_clima, by = "fecha")

# Contamos la cant de veces que se usó cada modelo de bicicleta
modelo_counts <- df_joineado %>%
  group_by(modelo_bicicleta) %>%
  summarise(count = n())

# Creamos el gráfico de barras con colores personalizados
ggplot(modelo_counts, aes(x = modelo_bicicleta, y = count, fill = modelo_bicicleta)) +
  geom_bar(stat = "identity") +
  labs(x = "Modelo de Bicicleta", y = "Cantidad de Usos", title = "Uso de Modelos de Bicicleta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black", size = 12, face = "bold"),
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5), # Ajuste para centrar el título
        legend.position = "none") +
  scale_fill_manual(values = c("FIT" = "orange", "ICONIC" = "#6baed6", "OTHER" = "#78c679"))  # Colores personalizados






#########  MODELO DE BICI AGRUPADO POR GENERO ######

# Agrupamos por modelo de bicicleta y género, contamos la cantidad de usuarios
modelo_genero_counts <- df_bici %>%
  group_by(modelo_bicicleta, genero) %>%
  summarise(count = n())



# Creamos el gráfico de barras apiladas con colores personalizados
ggplot(modelo_genero_counts, aes(x = modelo_bicicleta, y = count, fill = genero)) +
  geom_bar(stat = "identity") +
  labs(x = "Modelo de Bicicleta", y = "Cantidad de Usuarios", title = "Distribución de Modelos por Género") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black", size = 12, face = "bold"),
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("MALE" = "#6baed6", "FEMALE" = "#f768a1", "OTHER" = "#78c679"))  # Colores personalizados

# LA PROPORCION MAS O MENOS SE MANTIENE



###### MODELO POR DIA DE SEMANA ######

library(ggplot2)
library(dplyr)
library(lubridate)

# Convertir la columna de fecha a formato Date
df_bici$fecha <- as.Date(df_bici$fecha)

# Agregar una columna para el día de la semana
df_bici <- df_bici %>%
  mutate(dia_semana = weekdays(fecha))

# Calcular la cantidad de cada modelo por día de la semana
modelo_por_dia <- df_bici %>%
  group_by(dia_semana, modelo_bicicleta) %>%
  summarise(count = n())

# Crear el gráfico de barras apiladas
ggplot(modelo_por_dia, aes(x = dia_semana, y = count, fill = modelo_bicicleta)) +
  geom_bar(stat = "identity") +
  labs(x = "Día de la Semana", y = "Cantidad de Usos", title = "Modelo de bicicleta por dia de la semana") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black", size = 12, face = "bold"),
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("FIT" = "orange", "ICONIC" = "#6baed6", "OTHER" = "#78c679"))  # Colores personalizados


#-------------------#
  
# Left join de df_bici con df_clima por la columna "fecha"
df_joineado <- left_join(df_bici, df_clima, by = "fecha")

# Graficar la cantidad de usuarios únicos por género
usuarios_por_genero <- df_joineado %>% 
  group_by(genero) %>% 
  summarize(cant_usuarios = n_distinct(id_usuario))

# Grafico de cantidad de usuarios únicos por género
ggplot(usuarios_por_genero, aes(x = genero, y = cant_usuarios, fill = genero)) +
  geom_bar(stat = "identity") +
  labs(x = "Género", y = "Cantidad de usuarios únicos") +
  ggtitle("Cantidad de usuarios únicos por género") +
  scale_fill_manual(values = c("MALE" = "#6baed6", "FEMALE" = "#f768a1", "OTHER" = "#78c679")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))





############## Cantidad de usuarios vs usuarios unicos ########


# Calculamos la cantidad de viajes por género
viajes_por_genero <- df_bici %>%
  group_by(genero) %>%
  summarise(cantidad_viajes = n())

# Calculamos la cantidad de usuarios únicos por género
usuarios_unicos_por_genero <- df_bici %>%
  distinct(id_usuario, .keep_all = TRUE) %>%
  group_by(genero) %>%
  summarise(cantidad_usuarios = n_distinct(id_usuario))

# Combinamos los datos
datos_comparacion <- merge(viajes_por_genero, usuarios_unicos_por_genero, by = "genero")


ggplot(datos_comparacion, aes(x = genero)) +
  geom_bar(aes(y = cantidad_viajes, fill = "Cantidad de Viajes"), stat = "identity", width = 0.4) +
  geom_bar(aes(y = cantidad_usuarios, fill = "Usuarios Únicos"), stat = "identity", width = 0.4) +
  labs(title = "Comparación de Viajes y Usuarios Únicos por Género",
       y = "Cantidad",
       x = "Género",
       fill = "Variable") +
  theme_minimal() +
  scale_fill_manual(values = c("Cantidad de Viajes" = "#78c679", "Usuarios Únicos" = "black")) +
  theme(plot.title = element_text(color = "black", face = "bold", hjust = 0.5))


# Podemos observar que el género masculino tiene una mayor proporción de repetición de viajes



########## Franjas horarias ##########


# Esto es temporal, es solo para saber cual es la menor y mayor hora en que se viaja en bici.

# Convertimos la columna fecha_origen_recorrido a formato de fecha
df_bici <- df_bici %>%
  mutate(fecha_origen_recorrido = as.POSIXct(fecha_origen_recorrido, format = "%Y-%m-%d %H:%M:%S"))

# Calculamos el menor y mayor horario de fecha_origen_recorrido
menor_horario <- min(df_bici$fecha_origen_recorrido)
mayor_horario <- max(df_bici$fecha_origen_recorrido)

# Imprimimos los resultados
print("Menor Horario:")
print(menor_horario)
print("Mayor Horario:")
print(mayor_horario)




# Creamos una nueva columna para la franja horaria. Utilizamos fecha_origen_recorrido, donde
# encontramos que el mayor horario max(fecha_origen_recorrido) era 05:10 am.y el mayor horario era 22:57pm,
# por lo cual decidimos definir 6 franjas equidistantes de 3 horas.

df_temp1 <- df_bici %>%
  mutate(franja_horaria = case_when(
    between(hour(as.POSIXct(fecha_origen_recorrido)), 5, 7) ~ "Franja 1: 5:00 AM - 8:00 AM",
    between(hour(as.POSIXct(fecha_origen_recorrido)), 8, 10) ~ "Franja 2: 8:00 AM - 11:00 AM",
    between(hour(as.POSIXct(fecha_origen_recorrido)), 11, 13) ~ "Franja 3: 11:00 AM - 2:00 PM",
    between(hour(as.POSIXct(fecha_origen_recorrido)), 14, 16) ~ "Franja 4: 2:00 PM - 5:00 PM",
    between(hour(as.POSIXct(fecha_origen_recorrido)), 17, 19) ~ "Franja 5: 5:00 PM - 8:00 PM",
    between(hour(as.POSIXct(fecha_origen_recorrido)), 20, 22) ~ "Franja 6: 8:00 PM - 11:00 PM",
    TRUE ~ "Otras franjas"
  ))

# Filtramos las filas nuestras franjas horarias
df_temp1 <- df_temp1 %>%
  filter(franja_horaria != "Otras franjas")

# Agrupamos por franja horaria y contamos la cantidad de viajes en cada franja
df_counts <- df_temp1 %>%
  group_by(franja_horaria) %>%
  summarise(cantidad_viajes = n())

# Creamos el gráfico de barras
ggplot(df_counts, aes(x = franja_horaria, y = cantidad_viajes)) +
  geom_bar(stat = "identity", fill = "#6baed6") +
  labs(x = " ", y = "Cantidad de Viajes", 
       title = "Cantidad de Viajes por Franja Horaria") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"), 
        axis.text.y = element_text(size = 10, face = "bold"),  
        plot.title = element_text(size = 16, face = "bold"),  
        plot.subtitle = element_text(size = 12),  
        legend.position = "none") 
