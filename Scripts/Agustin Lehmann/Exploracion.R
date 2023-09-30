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

#Solo se ve una diferencia los domingos pero eso se puede deber a que hay un poco mas de FIT que Iconic y no al dia en particular.

#cuantos viajes hay a lo largo del año?

df_bici %>%
  group_by(fecha) %>%
  summarise(
    n_viajes = n_distinct(id_recorrido)
  ) %>%
  ggplot(aes(x=fecha, y= n_viajes)) +
  geom_col() +
  geom_smooth() +
  scale_x_date(date_labels="%b", date_breaks  ="month")
  
#la cantidad de viajes baja en verano e invierno(enero y julio como los menores) y aumenta en primavera otoño (abril y oct-nov)

#las precipitaciones afectan?
df_bici %>%
  left_join(df_clima, "fecha") %>%
  group_by(fecha) %>%
  mutate(
    n_viajes = n_distinct(id_recorrido),
  ) %>%
  ungroup() %>%
  ggplot(aes(x=lluvias_mm, y=n_viajes)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  facet_wrap(vars(dia_semana=weekdays(fecha))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) 

#a mayor pptacion menos viajes hay

#que estaciones son las mas populares (estacion de destino), cual tiene mas viajes?

df_bici %>%
  group_by(nombre_estacion_destino) %>%
  summarize(
    n_viajes = n_distinct(id_recorrido)
  ) %>% 
  arrange(desc(n_viajes)) %>%
  top_n(10, n_viajes) %>% 
  mutate(
    str_wrap(nombre_estacion_destino, width = 5)
  ) %>%
  ggplot(aes(x=nombre_estacion_destino, y= n_viajes, level=nombre_estacion_destino)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#constitucion, pacifico y congreso son las mas populares

#esto es todo el año o cambia mes a mes?

plot_estacion_destino_mes = function(mes){
return(df_bici %>%
  filter(lubridate::month(fecha_destino_recorrido)== lubridate::month(mes)) %>%
  group_by(nombre_estacion_destino) %>%
  summarise(
    n_viajes = n_distinct(id_recorrido),
    mes = lubridate::month(fecha_destino_recorrido)[1]
  ) %>% 
  arrange(desc(n_viajes)) %>%
  top_n(10, n_viajes) %>% 
  mutate(
    str_wrap(nombre_estacion_destino, width = 5)
  ) %>% 
  ggplot(aes(x=nombre_estacion_destino, y= n_viajes, level=nombre_estacion_destino))+
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(
      title = as.character(mes)
    ))
  }

map(1:12, ~plot_estacion_destino_mes(mes = .x))

#Varia mes a mes:
# en enero plaza irlanda fue la mas popular seguida de Julieta Lanteri
# en febrero Guatemala seguida de las estaciones F j Santamaria de oro, constitucion, godoy cruz y libertador
# en marzo, mayo congreso y constitucion
# abril , junio, diciembre constitucion
# julio azucena, malabia
# agosto Retiro Pacifico Godoy cruz y libertador
# septiembre, oct pacifico
# Nov Facultad derecho Acuña de figueroa constitucion rodrigo bueno , godoycruz y libertador

#distribucion de viajes origen/destino en el mapa

require(sf)
require(plotly)
library(showtext) # https://r-graph-gallery.com/custom-fonts-in-R-and-ggplot2.html
library(cowplot)
#le hice ingenieria inversa a esto https://github.com/z3tt/TidyTuesday/blob/main/R/2020_05_TreesSF.Rmd para el mapa del fondo
#el shapefile de las calles https://data.buenosaires.gob.ar/dataset/calles/resource/juqdkmgo-302-resource , 
#el de ciclovias/comunas es de la misma pagina pero perdi el link

font_add("PlexSans",
         italic = "Assets/fonts/IBM_Plex_Sans/IBMPlexSans-Light.ttf" ,
         bold="Assets/fonts/IBM_Plex_Sans/IBMPlexSans-Bold.ttf",
         regular ="Assets/fonts/IBM_Plex_Sans/IBMPlexSans-Regular.ttf" )
showtext_auto()

unzip("Assets/shapefiles/mapa_calles/callejero.zip", exdir = "Assets/shapefiles/mapa_calles")

mapa = st_read("Assets/shapefiles/mapa_calles") %>% 
  st_transform( "+proj=longlat +ellps=WGS84 +datum=WGS84") #para q se acomode al scatter source: google

comunas = st_read("Assets/shapefiles/comunas")%>% 
  st_transform( "+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  arrange(COMUNAS)%>%
  mutate(COMUNAS = factor(as.character(COMUNAS),as.character(COMUNAS))) #ordeno


df_bici %>%
  group_by(nombre_estacion_origen) %>%
  mutate (
    n_partidas = n_distinct(id_recorrido)
  ) %>%
  ggplot() +
  geom_sf(data= mapa, alpha = 0.3 ) +
  geom_point(aes(x=long_estacion_origen, y=lat_estacion_origen, size=n_partidas), alpha = 0.7)

theme_set(theme_minimal())

theme_update(
             plot.background = element_rect(fill = "#0b132b", color = "#0b132b"),
             panel.background = element_rect(fill = "#0b132b", colour = "#0b132b"),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             axis.ticks = element_blank(),
             legend.text = element_text(color = "white"),
             legend.title = element_text(color = "white"),
             
             
             )
# 
# ggplotly(
df_bici %>%
  group_by(nombre_estacion_destino) %>%
  mutate (
    n_dest = n_distinct(id_recorrido)
  ) %>%
  ggplot(aes(text=nombre_estacion_destino), alpha =0.7) +
  geom_sf(data= mapa, alpha = 0.3 ,aes(text=NULL),color="white") +
  geom_point(aes(x=long_estacion_destino, y=lat_estacion_destino, size=n_dest* 1.25 ), color="black") +
  geom_point(aes(x=long_estacion_destino, y=lat_estacion_destino,
                 size=n_dest *0.5,
                 color=n_dest,
                 ),
             alpha = 0.1)+
  scale_color_gradient(high="turquoise",low="#3a606b",guide=F)+
  scale_size(name="N° de viajes", range(1:125), breaks = c(20,40,80,120), labels= c(20,40,80,120), guide=F) +
  guides(size="none")+
  guides(size = guide_legend(label.position = "bottom", 
                             override.aes = list(color = c("#3A506B","#4B8895","#5BC0BE","#6FFFE9"), stroke = .8, fill = NA),
                             title.position="top",

                             )
         ) +
  theme(
    legend.position = c(.15, .08),
    legend.direction = "horizontal",
    legend.key.width = unit(.01, "lines"),
    legend.text = element_text(size = 8, family = "PlexSans Italic", color = "grey80"),
    legend.title = element_text(family ="PlexSans Regular", hjust=-0.3),
    plot.title = element_text(size = 18, family ="PlexSans Regular", color = "white"),
    plot.subtitle = element_text(size = 12, family = "PlexSans Italic", color = "grey80"),
    plot.margin = margin(10, 30, 10, 50),
   ) +
  labs(
    title = "Estaciones de Destino",
    subtitle =  "Todo 2022"
  ) +
  annotate( "text",
    x=-58.423 , y = -34.692,
    label = str_wrap("Las 6 estaciones mas populares fueron: Constitucion (125), Pacifico (106), Congreso (95), Godoy Cruz Y Libertador (95) , Plaza Italia (85) y Hospital De Clinicas (85)"
                     ,width=40),
    color = "white",
    family ="PlexSans Bold",
    hjust = 0
  )


plot_destino = function(df, texto, subtitulo = "Todo 2022"){
  
  theme_set(theme_minimal())
  
  theme_update(
    plot.background = element_rect(fill = "#0b132b"),
    panel.background = element_rect(fill = "#0b132b", colour = "#0b132b"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
    )
  
  grafico = df %>%
    group_by(nombre_estacion_destino) %>%
    mutate (
      n_dest = n_distinct(id_recorrido)
    ) %>%
    ggplot(aes(text=nombre_estacion_destino), alpha =0.7) +
    geom_sf(data= mapa, alpha = 0.3 ,aes(text=NULL),color="white") +
    geom_point(aes(x=long_estacion_destino, y=lat_estacion_destino, size=n_dest* 1.25 ), color="black") +
    geom_point(aes(x=long_estacion_destino, y=lat_estacion_destino,
                   size=n_dest *0.5,
                   color=n_dest,
    ),
    alpha = 0.1)+
    scale_color_gradient(high="turquoise",low="#3a606b",guide=F)+
    scale_size(name="N° de viajes", range(1:125), breaks = c(20,40,80,120), labels= c(20,40,80,120), guide=F) +
    guides(size="none")+
    guides(size = guide_legend(label.position = "bottom", 
                               override.aes = list(color = c("#3A506B","#4B8895","#5BC0BE","#6FFFE9"), stroke = .8, fill = NA),
                               title.position="top",
                               
    )
    ) +
    theme(
      legend.position = c(.15, .08),
      legend.direction = "horizontal",
      legend.key.width = unit(.01, "lines"),
      legend.text = element_text(size = 8, family = "PlexSans Italic", color = "grey80"),
      legend.title = element_text(family ="PlexSans Regular", hjust=0.3),
      plot.title = element_text(size = 18, family ="PlexSans Regular", color = "white"),
      plot.subtitle = element_text(size = 12, family = "PlexSans Italic", color = "grey80"),
      ) +
    labs(
      title = "Estaciones de Destino",
      subtitle = subtitulo
    ) +
    annotate( "text",
              x=-58.413 , y = -34.682,
              label = str_wrap(texto ,width=30),
              color = "white",
              family ="PlexSans Bold",
              hjust = 0,
              size = 6.7,
              lineheight = .6
    )
  
    
  return( grafico)
    
}

# tidyverse https://www.tidyverse.org/blog/2020/08/taking-control-of-plot-scaling/
p = plot_destino(df_bici, texto = "Las 6 estaciones mas populares fueron: Constitucion (125), Pacifico (106), Congreso (95), Godoy Cruz Y Libertador (95) , Plaza Italia (85) y Hospital De Clinicas (85)")

require(ragg)
ggsave(
  "Assets/img/Estaciones destino Año.png", 
  p, 
  device = agg_png, 
  width = 12, height = 12, units = "cm", res = 900,
  scaling = 0.5
)
knitr::include_graphics("Estaciones destino Año.png")

g = df_bici %>%
  group_by(month = lubridate::floor_date(fecha_origen_recorrido, 'month')) %>%
  plot_destino(texto= "",subtitulo = "Mes a Mes") +
  facet_wrap(vars(month), nrow=2) +
  theme(
    strip.text.x = element_text(
      size = 7, color = "white", family ="PlexSans Regular"
    ))

g

ggsave(
  "Assets/img/Estaciones destino Mes a mes.png", 
  g, 
  device = agg_png, 
  width =20, height = 8, units = "cm", res = 1900,
  scaling = 0.3
)
knitr::include_graphics("Estaciones destino Año.png")
