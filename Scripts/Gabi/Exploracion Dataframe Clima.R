require(tidyverse)


df_clima = read_csv('Data/Clima_limpio.csv')
View(df_clima)


# Carga y visualización del dataframe
# Suponiendo que tienes tu dataframe df_clima ya cargado

# Convertir la columna 'fecha' a tipo Date
df_clima$fecha <- as.Date(df_clima$fecha)

# Extraemos el mes de la columna 'fecha'
df_clima$mes <- format(df_clima$fecha, "%m")

#  Convertimos a tipo numerico
df_clima$mes <- as.numeric(df_clima$mes)

# Agrupamos por mes y calculamos los promedios de cada columna
df_promedios <- df_clima %>%
  group_by(mes) %>% summarize(temp_max_prom = mean(temp_max),
            temp_min_prom = mean(temp_min),
            lluvias_mm_prom = mean(lluvias_mm),
            viento_veloc_prom = mean(viento_veloc),
            presion_prom = mean(presion))

# Comencemos a visualizar los valores


# Promedio mensual de la temperatura maxima

ggplot(df_promedios, aes(x = mes, y = temp_max_prom)) +
  geom_point(color = "red", size = 3) +
  labs(x = "Mes", y = "Promedio de Temperatura Máxima", title = "Promedio mensual de Temperatura Máxima") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") # Para que el titulo este en negrita
  )

# Promedio mensual de la temperatura minima

ggplot(df_promedios, aes(x = mes, y = temp_min_prom)) +
  geom_point(color = "blue", size = 3) +
  labs(x = "Mes", y = "Promedio de Temperatura Mínima", title = "Promedio mensual de Temperatura Mínima") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Promedio mensual de las lluvias

ggplot(df_promedios, aes(x = mes, y = lluvias_mm_prom)) +
  geom_point(color = "green", size = 3) +
  labs(x = "Mes", y = "Promedio de Lluvias (mm)", title = "Promedio mensual de Lluvias (mm)") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


# Promedio mensual de la velocidad del viento

ggplot(df_promedios, aes(x = mes, y = viento_veloc_prom)) +
  geom_point(color = "purple", size = 3) +
  labs(x = "Mes", y = "Promedio de velocidad del viento", title = "Promedio mensual de velocidad del viento") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Promedio mensual de la presion atmosferica

ggplot(df_promedios, aes(x = mes, y = presion_prom)) +
  geom_point(color = "orange", size = 3) +
  labs(x = "Mes", y = "Promedio de Presión", title = "Promedio mensual de Presión Atmosferica") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



# Este grafico tiene la dispersion para todas las variables juntas (excepto la presion porque se iba de rango)
ggplot(df_promedios, aes(x = mes)) +
  geom_point(aes(y = temp_max_prom, color = "Temp Max"), size = 3) +
  geom_point(aes(y = temp_min_prom, color = "Temp Min"), size = 3) +
  geom_point(aes(y = lluvias_mm_prom, color = "Lluvias (mm)"), size = 3) +
  geom_point(aes(y = viento_veloc_prom, color = "Viento Velocidad"), size = 3) +
  # geom_point(aes(y = presion_prom, color = "Presión"), size = 3) +
  labs(x = "Mes", y = "Promedios", color = "Variables", title = "Promedio mensual") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  scale_color_manual(values = c("Temp Max" = "red", "Temp Min" = "blue", "Lluvias (mm)" = "green", "Viento Velocidad" = "purple", "Presión" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )









