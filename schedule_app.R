# schedule data download and app deployment

library(tidyverse)
library(readxl)
library(lubridate)
library(rsconnect)

url <- "https://www.foedevarestyrelsen.dk/_layouts/15/sdata/smileystatus.xlsx"

destfile <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/smileystatus_",today(),".xlsx")

download.file(url = url, destfile = destfile)

df <- readxl::read_excel(glue::glue("smileystatus_{today()}.xlsx")) %>%
  dplyr::select(navn1,cvrnr,adresse1,postnr,By,seneste_kontrol,seneste_kontrol_dato,URL,Geo_Lng,Geo_Lat) %>%
  group_by(cvrnr) %>% 
  arrange(desc(seneste_kontrol_dato)) %>% 
  top_n(1,seneste_kontrol_dato) %>%
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

write_csv(df, "smile/smiley_data.csv")

deployApp(appDir = "smile",launch.browser = FALSE,forceUpdate = T)