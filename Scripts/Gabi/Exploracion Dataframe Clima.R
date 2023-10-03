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


# Promedio mensual de la temperatura máxima con línea suave
ggplot(df_promedios, aes(x = mes, y = temp_max_prom)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +  # Agregamos la línea suave
  labs(x = "Mes", y = "Promedio de Temperatura Máxima", title = "Promedio mensual de Temperatura Máxima") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Haz lo mismo para las otras gráficas, agregando líneas suaves a cada una

# Promedio mensual de la temperatura mínima con línea suave
ggplot(df_promedios, aes(x = mes, y = temp_min_prom)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1) +
  labs(x = "Mes", y = "Promedio de Temperatura Mínima", title = "Promedio mensual de Temperatura Mínima") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Promedio mensual de las lluvias con línea suave
ggplot(df_promedios, aes(x = mes, y = lluvias_mm_prom)) +
  geom_point(color = "green", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "green", size = 1) +
  labs(x = "Mes", y = "Promedio de Lluvias (mm)", title = "Promedio mensual de Lluvias (mm)") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Promedio mensual de la velocidad del viento con línea suave
ggplot(df_promedios, aes(x = mes, y = viento_veloc_prom)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "purple", size = 1) +
  labs(x = "Mes", y = "Promedio de velocidad del viento", title = "Promedio mensual de velocidad del viento") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Promedio mensual de la presión atmosférica con línea suave
ggplot(df_promedios, aes(x = mes, y = presion_prom)) +
  geom_point(color = "orange", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "orange", size = 1) +
  labs(x = "Mes", y = "Promedio de Presión", title = "Promedio mensual de Presión Atmosférica") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Promedio mensual para todas las variables con líneas suaves
ggplot(df_promedios, aes(x = mes)) +
  geom_point(aes(y = temp_max_prom, color = "Temp Max"), size = 3) +
  geom_point(aes(y = temp_min_prom, color = "Temp Min"), size = 3) +
  geom_point(aes(y = lluvias_mm_prom, color = "Lluvias (mm)"), size = 3) +
  geom_point(aes(y = viento_veloc_prom, color = "Viento Velocidad"), size = 3) +
  geom_smooth(aes(y = temp_max_prom, color = "Temp Max"), method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = temp_min_prom, color = "Temp Min"), method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = lluvias_mm_prom, color = "Lluvias (mm)"), method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = viento_veloc_prom, color = "Viento Velocidad"), method = "loess", se = FALSE, size = 1) +
  labs(x = "Mes", y = "Promedios", color = "Variables", title = "Promedio mensual") +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  scale_color_manual(values = c("Temp Max" = "red", "Temp Min" = "blue", "Lluvias (mm)" = "green", "Viento Velocidad" = "purple")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
