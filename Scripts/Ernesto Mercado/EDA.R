require(tidyverse)

df_bici = read_csv('Data/Bicis_limpio.csv')
View(df_bici)

df_clima = read_csv('Data/Clima_limpio.csv')

#hora de salida por modelo y dia
df_bici %>%
  mutate(hora_de_salida=lubridate::hour(fecha_origen_recorrido)) %>%
  ggplot(mapping = aes(x=hora_de_salida))+
  geom_line(stat="count") +
  scale_x_continuous(breaks = seq(0,23,2)) +
  facet_wrap(modelo_bicicleta ~ (weekdays(fecha_origen_recorrido)))

#hora de salida por temperatura minima y dia
df_bici %>%
  left_join(df_clima) %>%
  mutate(hora_de_salida=lubridate::hour(fecha_origen_recorrido), aprox_temp = ifelse(temp_min < 10, "cold", ifelse(temp_min >= 10 & temp_min <= 18, "normal", "hot"))) %>%
  ggplot(mapping = aes(x=hora_de_salida)) +
  geom_line(stat="count") +
  scale_x_continuous(breaks = seq(0,23,2))+
  facet_wrap(~(aprox_temp))

#cantidad de trafico de bicicletas por genero en cada franja horaria
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

df_temp1 <- df_temp1 %>%
  filter(franja_horaria != "Otras franjas")

df_temp1 %>%
  ggplot(aes(x=franja_horaria))+
  geom_bar() +
  facet_wrap(~genero)