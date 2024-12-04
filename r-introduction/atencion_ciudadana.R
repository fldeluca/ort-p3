#Libraries
library(tidyverse)

#Datasets
atencion_ciudadano <- read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/sistema-unico-de-atencion-ciudadana/gcba_suaci_barrios.csv", stringsAsFactors = TRUE)
barrios_comunas <- read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios_comunas_p_Ciencia_de_Datos_y_PP.csv")

atencion_ciudadano <- left_join(atencion_ciudadano, barrios_comunas)
write.csv(atencion_ciudadano, "atencion_ciudadano.csv", row.names = FALSE)

#Filters 
seleccion <- select(atencion_ciudadano, PERIODO, total)
seleccion <- filter(atencion_ciudadano, BARRIO == "RETIRO")
seleccion <- filter(atencion_ciudadano, BARRIO == "RETIRO" | BARRIO =="PALERMO")
seleccion <- filter(atencion_ciudadano, !(TIPO_PRESTACION == "DENUNCIA" & RUBRO == "SEGURIDAD E HIGIENE"))
ordenado <- arrange(atencion_ciudadano, total)
ordenado <- arrange(atencion_ciudadano, total, BARRIO)
atencion_ciudadano <- mutate(atencion_ciudadano, AÑO = substr(PERIODO, 1, 4), MES = substr(PERIODO, 5, 6))

summarise(atencion_ciudadano, promedio = mean(total))

agrupado <- group_by(atencion_ciudadano, AÑO, MES)
sumario <- summarise(agrupado, promedio = mean(total))
head(sumario)

atencion_ciudadano %>%
  filter(AÑO == 2014) %>%
  group_by(BARRIO) %>%
  summarise(total = sum(total)) %>%
  arrange(desc(total)) %>%
  head(5)

atencion_ciudadano %>% filter(AÑO == 2014)

contactos_por_comuna <- atencion_ciudadano %>%
  group_by(COMUNA) %>%
  summarise(miles_contactos = sum(total) / 1000 )

habitantes <- read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/gcba_pob_comunas_17.csv")

contactos_por_comuna <- contactos_por_comuna %>% left_join(habitantes)

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos, color = factor(COMUNA), size = 3))

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total, fill = TIPO_PRESTACION)) +
  coord_flip()

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = TIPO_PRESTACION, weight = total))

contactos_por_mes_y_tipo <- atencion_ciudadano %>%
  group_by(PERIODO, TIPO_PRESTACION) %>%
  summarise(gran_total = sum(total))

ggplot(contactos_por_mes_y_tipo) +
  geom_histogram(aes(x = gran_total)) +
  facet_wrap(~TIPO_PRESTACION)

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total, fill = TIPO_PRESTACION)) +
  coord_flip() +
  labs(title = "Contactos realizados al Sistema Único de Atención Ciudadana",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2013 - 2015",
       caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
       x = "barrio",
       y = "cantidad",
       fill = "Motivo del contacto")

