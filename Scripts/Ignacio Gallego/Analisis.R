require(tidyverse)

df_clima = read_csv('Data/Clima_limpio.csv')
View(df_clima)

df_bici = read_csv("Data/Bicis_limpio.csv")
View(df_bici)

#Hay relacion entre la duracion del viaje y el modelo de la bici?

ggplot(data= df_bici,mapping= aes(x=modelo_bicicleta,y=duracion_recorrido))+
  geom_point()+
  geom_boxplot()+
  labs(title= "Tiempo de viaje en funcion del modelo de bicicleta",x="Modelo de Bicicleta",y="Tiempo de viaje (s)")

#por los graficos no se ve una diferencia
#veamos los datos resumen

df_bici %>%
  group_by(modelo_bicicleta) %>%
  summarise(
    promedio_duracion=mean(duracion_recorrido),
    mediana_duracion=median(duracion_recorrido),
    desvioS_duracion=sd(duracion_recorrido)
  )

#vemos que no hay una diferencia significativa entre los modelos de bicicleta.


#A que hora se realizan mas viajes?

df_bici %>%
  mutate(hora_de_salida=lubridate::hour(fecha_origen_recorrido)) %>%
  ggplot(mapping = aes(x=hora_de_salida))+
  geom_line(stat="count")+
  scale_x_continuous(breaks = seq(0,23,by=2))

#y mes a mes:

df_bici %>%
  mutate(hora_de_salida=lubridate::hour(fecha_origen_recorrido)) %>%
  ggplot(mapping = aes(x=hora_de_salida))+
  geom_line(stat="count")+
  scale_x_continuous(breaks = seq(0,23,by=2))+
  facet_wrap(vars(lubridate::month(fecha_origen_recorrido,label=TRUE)))


#por dia de la semana:

df_bici %>%
  mutate(hora_de_salida=lubridate::hour(fecha_origen_recorrido)) %>%
  ggplot(mapping = aes(x=hora_de_salida))+
  geom_line(stat="count")+
  scale_x_continuous(breaks = seq(0,23,by=2))+
  facet_wrap(vars(weekdays(fecha_origen_recorrido)),levels = as.factor(c("lunes","martes","miércoles","jueves","viernes","sabado","domingo")))

