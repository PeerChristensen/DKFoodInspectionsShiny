#Plotly map

library(tidyverse)
library(rnaturalearth)
library(readxl)
library(plotly)

dk <- ne_states(country = "Denmark", returnclass = "sf")
plot_ly(data,type="scattergeo")

plot_geo(data,lat = ~lat, lon = ~lon,locationmode = 'country names')

colours <- c("#00cc00", "#e8eb0c", "#f77806", "#e60000")

ge <- list(
  scope = 'europe',
  showland = T,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  zoom = 5,
  center = list(lat = ~median(lat), lon = ~median(lon))
)

Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoicGVlcmNocmlzdGVuc2VuIiwiYSI6ImNrYjIwaWdlaTA2Z2oyeG8yZTBuZTZqdnQifQ.0Eyq0G9gkoOmCZKfev4LHQ")

plot_mapbox(data=data, type = "scatter", x=~lon,y=~lat,
            color = ~factor(seneste_kontrol),colors=colours,
            text = ~paste("Navn:", navn1,
                          "<br>CVR :", cvrnr,
                          "<br>Adresse :",adresse1,
                          "<br>Postnr : ",postnr,
                          "<br>By :",By,
                          "<br>Seneste kontrol: ",seneste_kontrol_dato,
                          "<br><a href=",data$URL,'>Rapport</a>'),
            textposition = "auto",
            hoverinfo = "text") %>% 
  layout(mapbox = ge)
