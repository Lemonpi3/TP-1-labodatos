require(tidyverse)

df_bici = read_csv('Data/Bicis_limpio.csv')
View(df_bici)

palette = c("#f64f2a", "#d4cc21", "#80b918", "#71e2e5", "#2d728f", "#ffffff", "#000000")

#por cuanto tiempo duran los recorridos normalmente durante todo el año?
df_bici %>%
ggplot(aes(y=duracion_recorrido)) +
  geom_boxplot()

#veo el promedio y mediana
mean(df_bici$duracion_recorrido)
median(df_bici$duracion_recorrido)

#Parece q hay outliers arriba veamos apartir de que valor y cuantos son
upper_fence = quantile(df_bici$duracion_recorrido, p=0.75) +1.5*IQR(df_bici$duracion_recorrido)
upper_fence
sum(df_bici$duracion_recorrido > upper_fence)

# Hay 348 outliers con mas de 2684.5 segundos de recorrido, osea que la mayoria 
# de las personas usa las bicis por menos de 45min. 
# El tiempo promedio de uso es de 18 mins 42segs.
# El tiempo medio de uso es de 16mins.

# Y por mes?
df_bici %>%
  group_by(month = lubridate::floor_date(fecha_origen_recorrido, 'month')) %>% #saco el mes
  summarize(
    upper_fence = quantile(duracion_recorrido, p=0.75) +1.5*IQR(duracion_recorrido),
    lower_fence = quantile(duracion_recorrido, p=0.75) -1.5*IQR(duracion_recorrido),
    mean_time = mean(duracion_recorrido),
    median_time = median(duracion_recorrido),
    max_time = max(duracion_recorrido)
  ) %>%
  ggplot(aes(x=month)) +
  geom_line(aes(y=max_time, color =palette[5])) +
  geom_line(aes(y=upper_fence, color =palette[4])) +
  geom_line(aes(y=median_time, color=palette[3])) +
  geom_line(aes(y=mean_time, color=palette[2])) +
  geom_line(aes(y=lower_fence, color=palette[1])) +
  scale_color_manual( 
    name = "Duracion del recorrido", 
    values = palette[1:5], labels = c("Maximo", "Upper fence","Media","Promedio","Lower fence"),
    guide = guide_legend(direction = "vertical", title.position = "top", title.hjust = 0.5)) +
  scale_x_datetime(date_labels="%b", date_breaks  ="month")

# parece q en invierno el promedio y la media de duracion de recorridos es mas baja.
# En abril se ve q hubo una bajada en el tiempo de recorrido maximo y upper fence.



