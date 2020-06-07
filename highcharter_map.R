# create map with highcharter
library(highcharter)
library(tidyverse)

dk <- hcmap("countries/dk/dk-all")
write_rds(dk,"dk_map")
dk <- read_rds("dk_map")

df <- read_csv("smiley_data.csv") %>%
  mutate(lon= Geo_Lng,
         lat = Geo_Lat) %>%
  select(-Geo_Lng,-Geo_Lat) %>%
  filter(!is.na(lon),
         lat > 0) %>%
  mutate(colour = case_when(seneste_kontrol <= 2 ~ "#00cc00",
                            seneste_kontrol >= 3 ~ "#e60000")) %>%
  sample_n(1000)


x <- c("Navn", "CVR", "By","Adresse","Rapport")
y <- c("{point.navn1}", "{point.cvrnr}", "{point.By}", "{point.adresse1}", "{point.URL}")
tltip <- tooltip_table(x,y)

dk %>%
  hc_add_series(data = df, type = "mappoint" ,hcaes(color=colour),
                geojson = F, name = "Virksomhed",
                tooltip = list(pointFormat = tltip),
                marker = list(lineWidth = 0, radius = 4)) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_chart(backgroundColor = "#33caff") %>%
  hc_legend(enabled = F) %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) 


renderHighchart({
  
  d <- df %>% 
    filter(nkill>0,year == input$year_select) %>%
    select(country_txt,city,attacktype1_txt,longitude,latitude,nkill,summary) %>%
    mutate(location = glue::glue("{city}, {country_txt}"),
           summary = ifelse(is.na(summary),"none",summary),
           lat=latitude,
           lon=longitude,
           z = nkill) %>%
    select(location,attacktype1_txt,lat,lon,z,summary)
  
  x <- c("Location", "Attack type", "Casualties","Summary")
  y <- c("{point.location}", "{point.attacktype1_txt}", "{point.z}", "\n{point.summary}")
  tltip <- tooltip_table(x,y)
  
  mapdata %>%
    hc_add_series(data = d,
                  type = "mapbubble",
                  name="attack info",maxSize = '4%',
                  color="darkred") %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_chart(backgroundColor = "#1E1E1E") %>%
    hc_legend(enabled = F) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) 
  
})
