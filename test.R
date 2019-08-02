# Juli 2019
# 
# Beskrivelse:
# Dette script downloader dagligt smiley-registret fra Fødevarestyrelsen.
# Hver fil sammenlignes med foregående fil, og alle aktuelle opdateringer gemmes
# i delta-filer.
# Opdateringer videresendes via email.

Sys.setenv("HOME" = "/Users/peerchristensen/Desktop/Projects/Smiley_Data")
setwd("/Users/peerchristensen/Desktop/Projects/Smiley_Data")

print(date())

# ----- PAKKER ----------

library(tidyverse)
library(readxl)
library(RCurl)
library(lubridate)
library(cronR)
library(emayili)

# ----- DOWNLOAD FIL -----

url <- "https://www.foedevarestyrelsen.dk/_layouts/15/sdata/smileystatus.zip"

destfile <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/file_to_unzip_",today())

download.file(url = url, destfile = destfile)

unzip(destfile)

unlink(destfile)

df <- read_excel("SmileyStatus.xls")

write_csv(df, paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/smiley_rapport_",today(),".csv"))

# ----- FORBERED FILER -----

files <- list.files(pattern = "[[:digit:]].csv") %>%
  sort(decreasing = T)

file_dates <- files %>%
  map(function(x) substr(x, 16, 25)) %>%
  unlist() %>% 
  sort(decreasing=T) %>% 
  map(function(x) as.Date(x))

if (length(file_dates) >= 3) {
  file_dates_delete <- do.call("c",file_dates) %>%
    .[3:length(file_dates)]
  
  file_dates_delete %>% map(function(x) str_match_all(string=x,pattern=as.character(file_dates_delete))) %>% 
    unlist() %>%
    map(function(x) paste0("smiley_rapport_",x,".xls")) %>%
    unlist() %>% 
    map(function(x) unlink(x))
}

file_dates <- do.call("c",file_dates) %>%
  .[1:2]

# ----- LAV DELTA-FIL ------

files_compare <- files %>% 
  map(function(x) str_match_all(string=x,pattern=as.character(file_dates))) %>% 
  unlist() %>%
  map(function(x) paste0("smiley_rapport_",x,".csv")) %>%
  unlist()

no_delim <- function(x) str_remove_all(x,";")

df_1 <- read_csv(files_compare[2]) %>% #old
  mutate_all(as.character) %>%
  rename_all(no_delim) %>%
  mutate_all(no_delim)

df_2 <- read_csv(files_compare[1]) %>% #new
  mutate_all(as.character) %>%
  rename_all(no_delim) %>%
  mutate_all(no_delim)

#new <- sqldf('SELECT * FROM df_2 EXCEPT SELECT * FROM df_1') %>%
new <- dplyr::setdiff(df_2,df_1) %>%
  #filter(str_starts(postnr, "8"),seneste_kontrol >= 3) %>%
  mutate(kontrol_beskrivelse = case_when(seneste_kontrol == 1 ~ "ingen anmærkninger",
                                         seneste_kontrol == 2 ~ "indskærpelse",
                                         seneste_kontrol == 3 ~ "Påbud eller forbud",
                                         seneste_kontrol == 4 ~ "(sur smiley) bødeforlæg, politianmeldelse.."
  ))

new_filename <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/deltas/smiley_delta_",today(),".csv")

if (nrow(new) > 0) {
  write_csv(new, new_filename)
}
