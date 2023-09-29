require(tidyverse)

df_bici = read_csv('Data/Bicis_limpio.csv')
View(df_bici)


#Que dias son los que la gente mayormente anda en bici?
df_bici %>%
  mutate(dia_semana = weekdays(fecha_origen_recorrido)) %>%
  ggplot(aes(x=dia_semana)) +
  geom_bar()
#Los dias de semana suelen tener mayor trafico

#Los hombres conducen mas que las mujeres?
df_bici %>%
  ggplot(aes(x=genero)) +
  geom_bar()
#clara diferencia entre los dos

df_clima = read_csv('Data/Clima_limpio.csv')



View(df_clima)


summary(df_bici)