# download app data

library(tidyverse)
library(readxl)
library(lubridate)


url <- "https://www.foedevarestyrelsen.dk/_layouts/15/sdata/smileystatus.zip"

destfile <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/file_to_unzip_",today())

download.file(url = url, destfile = destfile)

unzip(destfile)

unlink(destfile)

df <- readxl::read_excel("SmileyStatus.xls") %>%
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

# send to File share
