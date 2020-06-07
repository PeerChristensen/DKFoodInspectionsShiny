#tmap map

library(sp)
library(GADMTools)
library(tidyverse)
library(tmap)

dk <- getData("GADM",country="DK",level=1)

data <- readxl::read_excel("SmileyStatus.xls") %>%
  dplyr::select(navn1,cvrnr,adresse1,postnr,By,seneste_kontrol,seneste_kontrol_dato,URL,Geo_Lng,Geo_Lat) %>%
  mutate(seneste_kontrol_dato = as.Date(seneste_kontrol_dato),
         Geo_Lng = as.numeric(Geo_Lng),
         Geo_Lat = as.numeric(Geo_Lat)) %>%
  filter(seneste_kontrol_dato >= lubridate::today()-365,
         Geo_Lng != 0,
         Geo_Lat != 0) %>%
  drop_na() %>%
  mutate(colour = case_when(seneste_kontrol == 1 ~ "#00cc00",
                            seneste_kontrol == 2 ~ "#e8eb0c",
                            seneste_kontrol == 3 ~ "#f77806",
                            seneste_kontrol == 4 ~ "#e60000")) %>%
  st_as_sf(coords = c("Geo_Lng", "Geo_Lat")) %>%
  st_set_crs(4326)

dk_sf <- st_as_sf(dk)
smiley_map <- st_join(data, dk_sf) 

tmap_mode("view")  # for an interactive leaflet map

tm_shape(smiley_map) +
  tm_bubbles(size=.05,col = "colour",alpha=.4,
             border.alpha=.1,
             popup.vars=c("CVR"="cvrnr", "Adresse"="adresse1",
                          "Postnr" = "postnr",
                          "Seneste kontrol" = "seneste_kontrol_dato",
                          "Rapport" = "URL"))



# Shiny
# in UI part:
leafletOutput("my_tmap")

# in server part
output$my_tmap = renderLeaflet({
  tm <- tm_shape(World) + tm_polygons("HPI", legend.title = "Happy Planet Index")
  tmap_leaflet(tm)
})