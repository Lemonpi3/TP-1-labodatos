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
# En abril se ve q hubo una bajada en el tiempo de recorrido maximo y upper fence

#Hay algun dia donde duren mas los recorridos?
df_bici %>%
  group_by(dia_semana = weekdays(fecha_origen_recorrido)) %>%
  ggplot(aes(y=duracion_recorrido, fill= dia_semana)) +
  geom_boxplot()

#Si, Los sabados y domingos suelen tener recorridos mas largos.

#El clima afecta la duracion de los recorridos?

df_clima = read_csv('Data/Clima_limpio.csv')

#lluvias x duracion

df_bici %>%
  left_join(df_clima, "fecha") %>%
  mutate(dia_semana =weekdays(fecha)) %>%
  ggplot(aes(y=duracion_recorrido, x = lluvias_mm, color=dia_semana)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = 'lm',se=F) +
  facet_wrap(vars(dia_semana))

#no estoy muy seguro de contarlo prefiero ignorarlo.

#temp max x duracion, la gente hace recorridos mas cortos o largos si hace mas calor?

df_bici %>%
  left_join(df_clima, "fecha") %>%
  mutate(dia_semana =weekdays(fecha)) %>%
  ggplot(aes(y=duracion_recorrido, x = temp_max, color=dia_semana)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = 'lm',se=F) +
  facet_wrap(vars(dia_semana))

#No cambia mucho asi que no se ve afectado.

#el modelo de bici afecta la duracion promedio, hacen que vayan mas rapido?
#primero veo si hay 50-50 de los modelos
df_bici %>%
  ggplot(aes(x=modelo_bicicleta)) +
  geom_bar()

#hay alrededor de 1000 FIT mas que ICONIC
#respondo la pregunta de arriba, separo por dia por que hay una diferencia en los findes como se vio antes.
df_bici %>%
  mutate(dia_semana =weekdays(fecha)) %>%
  group_by(modelo_bicicleta, dia_semana) %>%
  summarise(duracion_prom = mean(duracion_recorrido, na.rm = T)) %>%
  ggplot(aes(x=modelo_bicicleta, y=duracion_prom)) +
  geom_col() +
  facet_wrap(vars(dia_semana))

#Solo se ve una diferencia los domingos pero eso se puede deber a que hay un poco mas de FIT que Iconic.
  