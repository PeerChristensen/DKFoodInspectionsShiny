# leaflet

library(leaflet)
library(tidyverse)
library(htmltools)


leaflet()

data <- readxl::read_excel("SmileyStatus.xls") %>%
  dplyr::select(navn1,cvrnr,adresse1,postnr,By,seneste_kontrol,seneste_kontrol_dato,URL,Geo_Lng,Geo_Lat) %>%
  mutate(seneste_kontrol_dato = as.Date(seneste_kontrol_dato),
         lon = as.numeric(Geo_Lng),
         lat = as.numeric(Geo_Lat)) %>%
  filter(seneste_kontrol_dato >= lubridate::today()-365,
         Geo_Lng != 0,
         Geo_Lat != 0) %>%
  drop_na() %>%
  mutate(colour = case_when(seneste_kontrol == 1 ~ "#00cc00",
                            seneste_kontrol == 2 ~ "#e8eb0c",
                            seneste_kontrol == 3 ~ "#f77806",
                            seneste_kontrol == 4 ~ "#e60000"))

leaflet() %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
  addCircleMarkers(data = data, opacity = .5,fillOpacity = .9,radius=5, stroke = T, col = ~colour,
           label = ~navn1,
           popup = paste(
                        data$navn1,"<br>",
                        "CVR: ", data$cvrnr, "<br>",
                        data$adresse1,"<br>",
                        data$postnr,data$By, "<br>",
                        "Seneste kontrol: ",data$seneste_kontrol_dato, "<br>",
                        "<a href=",data$URL,'> Rapport</a>'),
           clusterOptions = markerClusterOptions())
  

           