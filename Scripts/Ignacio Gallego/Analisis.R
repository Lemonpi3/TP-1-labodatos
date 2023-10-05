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

df_bici %>%
  mutate(hora_de_salida=lubridate::hour(fecha_origen_recorrido)) %>%
  ggplot(mapping = aes(x=hora_de_salida))+
  geom_bar(stat="count",fill = "#6baed6")+
  scale_x_continuous(breaks = seq(0,23,by=2))+
  theme_minimal()+
  labs(x = "Hora de salida", y = "Cantidad de viajes", 
       title = "Cantidad de Viajes por Franja Horaria")
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

####Analisis clima

ggplot(df_clima,mapping = aes(x=fecha))+
  geom_line(mapping=aes(y=temp_prom),alpha=0.3)+
  geom_smooth(mapping=aes(y=temp_prom),color="grey2",se=F)+
  geom_smooth(mapping= aes(y=temp_min),se=F)+
  geom_smooth(mapping = aes(y=temp_max),color="red2",se=F)+
  scale_x_date(date_labels = "%b",date_breaks = "month")+
  labs(y="Temperatura (ºC)",x="Fecha",title="Temperatura a lo largo del año")+
  annotate("text",label="T Max",x=as.Date("2022-02-15"),y=28.5,color="red2",angle="-45")+
  annotate("text",label="T Min",x=as.Date("2022-02-01"),y=21.5,color="blue",angle="-45")+
  annotate("text",label="T Prom",x=as.Date("2022-02-10"),y=25,color="grey2",angle="-45")


#cantidad de viajes en función de la temperatura

#primero combino ambos datasets según la fecha y selecciono las columnas de temperatura y
#redondeo los decimales.

df_join <- df_bici %>%
  inner_join(df_clima,by="fecha") %>%
  select(temp_prom,temp_max,temp_min) %>%
  mutate(temp_prom= round(temp_prom),
         temp_max=round(temp_max),
         temp_min=round(temp_min))

#ahora grafico la cantidad de viajes según temperatura promedio, máxima, y mínima

ggplot(df_join,mapping=aes(x=temp_prom))+
  geom_histogram(mapping=aes(x=temp_prom),stat = "count",binwidth = 5,color="black",fill="aquamarine")+
  labs(title = "Viajes segun temperatura promedio",x="Temperatura (°C)",y="Cantidad de viajes")

ggplot(df_join,mapping=aes(x=temp_max))+
  geom_histogram(bins=5,stat = "count",color="black",fill="brown1")+
  labs(title = "Viajes segun temperatura máxima",x="Temperatura (°C)",y="Cantidad de viajes")

ggplot(df_join,mapping=aes(x=temp_min))+
  geom_histogram(stat = "count",fill="turquoise",color="black")+
  labs(title = "Viajes segun temperatura mínima",x="Temperatura (°C)",y="Cantidad de viajes")

ggplot(df_join)+
  geom_boxplot(mapping=aes(y=temp_prom,x="temperatura promedio"),fill="aquamarine")+
  geom_boxplot(mapping=aes(y=temp_max,x="temperatura maxima"),fill="brown1")+
  geom_boxplot(mapping=aes(y=temp_min,x="temperatura minima"),fill="turquoise")
  
