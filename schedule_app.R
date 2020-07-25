# schedule data download and app deployment

library(tidyverse)
library(readxl)
library(lubridate)
library(rsconnect)
library(glue)

if (wday(today()) < 6) {
print(now())

destfile <- "/Users/peerchristensen/Desktop/Projects/Smiley_data/smileystatus.xlsx"

unlink(destfile)

url <- "https://www.foedevarestyrelsen.dk/_layouts/15/sdata/smileystatus.xlsx"
 
#destfile <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/smileystatus_",today(),".xlsx")
#destfile <- "/Users/peerchristensen/Desktop/Projects/Smiley_data/app_data/smileystatus.xlsx"
download.file(url = url, destfile = destfile)

print("downloaded")

#date <- as.character(lubridate::today())

#filename <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/smileystatus_",date,".xlsx")

df <- readxl::read_excel("/Users/peerchristensen/Desktop/Projects/Smiley_data/smileystatus.xlsx") %>%
  dplyr::select(navn1,cvrnr,adresse1,postnr,By,seneste_kontrol,seneste_kontrol_dato,URL,Geo_Lng,Geo_Lat) %>%
  group_by(cvrnr) %>%
  arrange(desc(seneste_kontrol_dato)) %>%
  top_n(1,seneste_kontrol_dato) %>%
  ungroup() %>%
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

print("file read")

previous_most_recent <- read_csv("/Users/peerchristensen/Desktop/Projects/Smiley_data/smile/smiley_data.csv") %>%
  summarise(latest = max(seneste_kontrol_dato)) %>%
  pull(latest)

current_most_recent <- df %>%
  summarise(latest = max(seneste_kontrol_dato)) %>%
  pull(latest)

if (current_most_recent > previous_most_recent) {
  write_csv(df, "/Users/peerchristensen/Desktop/Projects/Smiley_data/smile/smiley_data.csv")

  print("csv written")

  options(rsconnect.check.certificate = FALSE)
  deployApp(appDir = "/Users/peerchristensen/Desktop/Projects/Smiley_Data/smile",
          launch.browser = T,
          forceUpdate = T)

  print("app deployed") } else {
    
    print("no updates")}

} else {print("weekend")}
