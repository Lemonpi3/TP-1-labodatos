require(tidyverse)
require(sf)
require(showtext)
#para juntar graficos en 1
require(cowplot) #mas manual
require(gridExtra) #automatico pero no necesariamente mas bonito

df_bici = read_csv('Data/Bicis_limpio.csv')
df_clima = read_csv('Data/Clima_limpio.csv')

#el archivo descomprimido era muy grande para github
unzip("Assets/shapefiles/mapa_calles/callejero.zip", exdir = "Assets/shapefiles/mapa_calles")

mapa = st_read("Assets/shapefiles/mapa_calles") %>% 
  st_transform( "+proj=longlat +ellps=WGS84 +datum=WGS84")

font_add_google('IBM Plex Sans',family = 'sans-serif')
font_add_google('IBM Plex Sans Condensed',family = 'sans-serif')
#fuentes
# font_add("PlexSans",
#          italic = "Assets/fonts/IBM_Plex_Sans/IBMPlexSans-Light.ttf" ,
#          bold="Assets/fonts/IBM_Plex_Sans/IBMPlexSans-Bold.ttf",
#          regular ="Assets/fonts/IBM_Plex_Sans/IBMPlexSans-Regular.ttf" )

showtext_auto()

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
              label = str_wrap(texto ,width=27),
              color = "white",
              family ="PlexSans Bold",
              hjust = 0,
              size = 4.9,
              lineheight = .8
    )
    return( grafico)
  
}

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

p = plot_destino(df_bici, texto = "")

b = df_bici %>%
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
  geom_col(fill=c("#affc41","#affc41","#affc41","grey80","grey80",
                  "grey80","grey80","grey80","grey80","grey80","grey80")) +
  geom_text(aes(label=n_viajes), vjust = 1.5, colour = "black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(
    title = "Top 10 Cantidad de viajes",
    subtitle = "Todo 2022"
  ) +
  theme(
    plot.title = element_text(size = 18, family ="PlexSans Regular", color = "white"),
    plot.subtitle = element_text(size = 12, family = "PlexSans Italic", color = "grey80"),
    plot.margin = margin(10, 30, 10, 50),
    axis.ticks.x = element_line(colour= "white"),
    axis.text.x = element_text(size = 8, family ="PlexSans Regular", color = "white"),
  ) 

#hago la slide

slide_destino_2022 = ggdraw() +
  draw_plot(ggplot(), x=0, y=0, width = 1, height = 1) + #fondo
  draw_plot(ggplot()+
              annotate("text",x=0.1,y=0.5,
                       label=str_wrap("Las 6 estaciones mas populares fueron: Constitucion (125), Pacifico (106), Congreso (95), Godoy Cruz Y Libertador (95) , Plaza Italia (85) y Hospital De Clinicas (85).
                                                                                 
                       Si bien las estaciones mas usadas estan por Palermo y el centro tomando todo el 2022,
esta tendendencia varia de mes a mes, veamoslo.", width = 60),
                       color = "white",
                       family ="PlexSans Bold",
                       hjust = 0) +
              scale_x_continuous(breaks=c(0,0.5,1)),
            x=0.2,y=0.5,width = 0.9, height = 0.5) +
  draw_plot(b, x = 0.58, y = 0, width = 0.4, height = 0.5) +
  draw_plot(p, x = -.18, y = 0, width = 1.1, height = 1) 

ggsave(
  "Assets/img/Slide_destino_2022.png", 
  slide_destino_2022, 
  device = agg_png, 
  width =20, height = 8, units = "cm", res = 1900,
  scaling = 0.3
)
knitr::include_graphics("Slide_destino_2022.png.png")  


plot_estacion_destino_mes = function(mes){
  meses = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  return(df_bici %>%
           filter(lubridate::month(fecha_destino_recorrido)== lubridate::month(mes)) %>%
           group_by(nombre_estacion_destino) %>%
           summarise(
             n_viajes = n_distinct(id_recorrido),
             mes = lubridate::month(fecha_destino_recorrido)[1]
           ) %>% 
           arrange(desc(n_viajes)) %>%
           top_n(10, n_viajes) %>% 
           head(10) %>%
           mutate(
             str_wrap(nombre_estacion_destino, width = 5),
             top_3 = ifelse((n_viajes>=n_viajes[3]), "Top 3", "Others"),
           ) %>% 
           ggplot(aes(x=nombre_estacion_destino, y= n_viajes, level=nombre_estacion_destino, fill = top_3))+
           geom_col() +
           geom_text(aes(label=n_viajes), vjust = 1.5, colour = "black") +
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          labs(
            title = "Top 10 Cantidad de viajes",
            subtitle = str_glue("{mes} 2022", mes =meses[mes])
          ) +
          theme(
            plot.title = element_text(size = 18, family ="PlexSans Regular", color = "white"),
            plot.subtitle = element_text(size = 12, family = "PlexSans Italic", color = "grey80"),
            plot.margin = margin(10, 30, 10, 50),
            axis.ticks.x = element_line(colour= "white"),
            axis.text.x = element_text(size = 8, family ="PlexSans Regular", color = "white"),
            legend.position = "none"
          ) +
           scale_fill_manual(values = c("Top 3" = "#affc41", "Others" = "gray80"),guide=F)
           
         )
}


graphs = map(1:6, ~plot_estacion_destino_mes(mes = .x))
g = grid.arrange(grobs = graphs)

ggsave(
  "Assets/img/Estaciones destino primer semestre.png", 
  g, 
  device = agg_png, 
  width =10, height = 6, units = "cm", res = 1900,
  scaling = 0.3
)
knitr::include_graphics("Estaciones destino primer semestre.png")

graphs = map(7:12, ~plot_estacion_destino_mes(mes = .x))
g = grid.arrange(grobs = graphs)

ggsave(
  "Assets/img/Estaciones destino segundo semestre.png", 
  g, 
  device = agg_png, 
  width =10, height = 6, units = "cm", res = 1900,
  scaling = 0.3
)
knitr::include_graphics("Estaciones destino segundo semestre.png")

#Lo mismo q para destino pero para estacion Origen

plot_origen = function(df, texto, subtitulo = "Todo 2022"){
  
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
    group_by(nombre_estacion_origen) %>%
    mutate (
      n_dest = n_distinct(id_recorrido)
    ) %>%
    ggplot(aes(text=nombre_estacion_origen), alpha =0.7) +
    geom_sf(data= mapa, alpha = 0.3 ,aes(text=NULL),color="white") +
    geom_point(aes(x=long_estacion_origen, y=lat_estacion_origen, size=n_dest* 1.25 ), color="black") +
    geom_point(aes(x=long_estacion_origen, y=lat_estacion_origen,
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
      legend.text = element_text(size = 8, family ='sans-serif', color = "grey80"),
      legend.title = element_text(family ='sans-serif', face = "bold", hjust=0.3),
      plot.title = element_text(size = 18, family ='sans-serif', face = "bold", color = "white"),
      plot.subtitle = element_text(size = 12, family = 'sans-serif', color = "grey80"),
    ) +
    labs(
      title = "Estaciones de Origen",
      subtitle = subtitulo
    ) +
    annotate( "text",
              x=-58.413 , y = -34.682,
              label = str_wrap(texto ,width=27),
              color = "white",
              family ='sans-serif',
              hjust = 0,
              size = 4.9,
              lineheight = .8
    )
  return( grafico)
  
}

p = plot_origen(df_bici, "")
p


b = df_bici %>%
  group_by(nombre_estacion_origen) %>%
  summarize(
    n_viajes = n_distinct(id_recorrido)
  ) %>% 
  arrange(desc(n_viajes)) %>%
  top_n(10, n_viajes) %>% 
  mutate(
    str_wrap(nombre_estacion_origen, width = 5)
  ) %>%
  ggplot(aes(x=nombre_estacion_origen, y= n_viajes, level=nombre_estacion_origen)) +
  geom_col(fill=c("turquoise","turquoise","turquoise","grey80","grey80",
                  "grey80","grey80","grey80","grey80","grey80")) +
  geom_text(aes(label=n_viajes), vjust = 1.5, colour = "black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(
    title = "Top 10 Cantidad de viajes",
    subtitle = "Todo 2022"
  ) +
  theme(
    plot.title = element_text(size = 18, family ='sans-serif',face='bold', color = "white"),
    plot.subtitle = element_text(size = 12, family = 'sans-serif', color = "grey80"),
    plot.margin = margin(10, 30, 10, 50),
    axis.ticks.x = element_line(colour= "white"),
    axis.text.x = element_text(size = 8, family ='sans-serif', color = "white"),
  ) 
b

slide_origen_2022 = ggdraw() +
  draw_plot(ggplot(), x=0.3, y=0, width = 0.9, height = 1) + #fondo
  draw_plot(ggplot()+
              annotate("text",x=0.1,y=0.5,
                       label=str_wrap("Los origenes mas comunes Fueron Constitucion, Pacifico Y Retiro
                       Si bien las estaciones mas usadas estan por Palermo y el centro tomando todo el 2022,
                       esta tendendencia varia de mes a mes, veamoslo.", width = 60),
                       color = "white",
                       family ='sans-serif',
                       fontface="bold",
                       hjust = 0,
                       size = 7) +
              scale_x_continuous(breaks=c(0,0.5,1)),
            x=0.2,y=0.5,width = 0.8, height = 0.5) +
  draw_plot(b, x = 0.57, y = 0, width = 0.4, height = 0.5) +
  draw_plot(p, x = -0.15, y = 0, width = 1.1, height = 1) 
slide_origen_2022

#guardo la slide
ggsave(
  "Assets/img/slide_origen_2022.png", 
  slide_origen_2022, 
  device = ragg::agg_png, 
  width =20, height = 8, units = "cm", res = 1900,
  scaling = 0.3
)
knitr::include_graphics("slide_origen_2022.png")  

#cant viajes por semana
df_bici %>%
  mutate(
    fecha = lubridate::ceiling_date(fecha, "week"),
    semana = lubridate::week(fecha)
  ) %>%
  group_by(semana) %>%
  summarise(
    n_viajes = n_distinct(id_recorrido),
    fecha =  format(fecha[1], "%d\n%b")
  ) %>%
  arrange(desc(n_viajes)) %>%
  mutate(
    top_5 = ifelse((n_viajes>=n_viajes[5]), "Top 5", ifelse((n_viajes<=n_viajes[48]), "Bottom 5", "Others")),
  ) %>% 
  ggplot(aes(x = semana, y = n_viajes)) +
  geom_col(aes(fill = top_5)) +
  geom_smooth(aes(y=n_viajes),se=F, color="#ffbe0b", size=1.5, method="gam", alpha = 0.1) +
  geom_text(aes(label = n_viajes), position = position_stack(vjust = 0.5), color = "black") +
  geom_text(aes(label = fecha, y = n_viajes + 1), position = position_stack(vjust = 1.06), color = "white") +
  scale_x_continuous(breaks = seq(from = 1, to = 53, by = 1)) +
  scale_fill_manual(values = c("Top 5" = "turquoise", "Others" = "gray80", "Bottom 5" = "#fb5607")) +
  guides(fill = FALSE) + 
  labs (
    title = "Cantidad de viajes 2022",
    subtitle = "Semanalmente"
  ) +
  theme(
    plot.title = element_text(size = 18, family ='sans-serif',face='bold', color = "white"),
    plot.subtitle = element_text(size = 12, family = 'sans-serif', color = "grey80"),
  )

#por cuanto tiempo duran los recorridos normalmente durante todo el año?
df_bici %>%
  ggplot(aes(y=duracion_recorrido)) +
  geom_boxplot(width=0.2, fill = "turquoise") +
  geom_segment(aes(x = -0.05, xend = 0.05, y = min(df_bici$duracion_recorrido), yend = min(df_bici$duracion_recorrido)),) +
  geom_segment(aes(x = -0.05, xend = 0.05, y = quantile(df_bici$duracion_recorrido, 0.75) + 1.5 * IQR(df_bici$duracion_recorrido), yend = quantile(df_bici$duracion_recorrido, 0.75) + 1.5 * IQR(df_bici$duracion_recorrido))) +
  annotate("text",
           x=0.08, y = quantile(df_bici$duracion_recorrido,0.75) + 50,
           label = lubridate::seconds_to_period(
             round(
               quantile(df_bici$duracion_recorrido,0.75), digits=2)
           )
  )+
  annotate("text",
           x=0.08, y = quantile(df_bici$duracion_recorrido,0.5) + 50,
           label = lubridate::seconds_to_period(
             round(
               quantile(df_bici$duracion_recorrido,0.5), digits=2)
           )
  )+
  annotate("text",
           x=0.08, y = quantile(df_bici$duracion_recorrido,0.25) + 50,
           label = lubridate::seconds_to_period(
             round(
               quantile(df_bici$duracion_recorrido,0.25), digits=2)
           )
  ) +
  annotate("text",
           x=0.078, y = quantile(df_bici$duracion_recorrido, 0.75) + 1.5 * IQR(df_bici$duracion_recorrido),
           label = lubridate::seconds_to_period(
             round(quantile(df_bici$duracion_recorrido, 0.75) + 1.5 * IQR(df_bici$duracion_recorrido),0)
           )
  ) +
  scale_x_continuous(breaks = c(-0.1,0,0.1)) +
  scale_y_continuous(breaks = seq(from=300, to=3600, by=600),labels = function(x) {
    return(lubridate::seconds_to_period(x)) } ) +
  labs(
    title = "Duracion del recorrido (segundos)"
  ) +
  theme_minimal() +
  theme(
    axis.title =element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )