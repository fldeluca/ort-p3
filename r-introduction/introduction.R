#Libraries
library(tidyverse)
library(sf)

#Datasets
suaci2018 <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sistema-unico-de-atencion-ciudadana/gcba_suaci_2018-1.csv")
barrios <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.geojson")
poblacion <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/caba_pob_barrios_2010.csv")

suaci2018 <- left_join(barrios, suaci2018)


#Plots
ggplot(suaci2018) +
  geom_col(aes(x = BARRIO, y = CONTACTOS)) +
  coord_flip()

ggplot(barrios) + 
  geom_sf(aes(fill = factor(comuna))) +
  
ggplot(barrios) +
  geom_sf(aes(fill = CONTACTOS / POBLACION)) +
  scale_fill_distiller(palette = "Spectral")
  

