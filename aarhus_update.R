# Opdateringer i smiley registret
# levering til Aarhus update
# filtre: postnummer starter med "8"
# Peer Christensen
# Juli 2019
# 
# Beskrivelse:
# Dette script downloader dagligt smiley-registret fra Fødevarestyrelsen.
# Hver fil sammenlignes med foregående fil, og alle aktuelle opdateringer gemmes
# i delta-filer.
# Opdateringer videresendes via email.

Sys.setenv("HOME" = "/Users/peerchristensen/Desktop/Projects/Smiley_Data")

# ----- PAKKER ----------
 
library(tidyverse)
library(readxl)
library(RCurl)
library(lubridate)
#library(sqldf)
library(cronR)
library(emayili)

# ----- DOWNLOAD FIL -----

 url <- "https://www.foedevarestyrelsen.dk/_layouts/15/sdata/smileystatus.zip"
# 
destfile <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/smiley_rapport_",today())
# 
download.file(url = url, destfile = destfile, method="libcurl",mode = "wb")
# 
 unzip(destfile)
# 
 destfile2 <- paste0(destfile,".csv")
# 
 file.rename(from = "SmileyStatus.xls", to = destfile2)

# ----- FORBERED FILER -----

files <- list.files(pattern = "[[:digit:]].xls") %>%
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
  map(function(x) paste0("smiley_rapport_",x,".xls")) %>%
  unlist()

setwd("/Users/peerchristensen/Desktop/Projects/Smiley_Data")
file_path = paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/",files_compare[1])

print("vheck")
print(files_compare[2])

df_1 <- read_excel(glue::glue("{files_compare[2]}")) #old
print("check2")
df_2 <- read_excel(files_compare[1]) #new

print("check1")

#new <- sqldf('SELECT * FROM df_2 EXCEPT SELECT * FROM df_1') %>%
new <- dplyr::setdiff(df_2,df_1) %>%
  mutate(kontrol_beskrivelse = case_when(seneste_kontrol == 1 ~ "ingen anmærkninger",
                                         seneste_kontrol == 2 ~ "indskærpelse",
                                         seneste_kontrol == 3 ~ "Påbud eller forbud",
                                         seneste_kontrol == 4 ~ "(sur smiley) bødeforlæg, politianmeldelse.."
)) %>%
  filter(str_starts(postnr, "8"),seneste_kontrol >= 3)

new_filename <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/deltas/smiley_delta_",today(),".csv")
print("check3")

if (nrow(new) > 0) {
  write_csv(new, new_filename)
}

#new %>% 
#  as_tibble() %>%
#  select(navn1,cvrnr, adresse1, By, seneste_kontrol, seneste_kontrol_dato, naestseneste_kontrol)

# ------ SEND RESULTATER -----

# error message function
error_message <- function(x) {
  
  library(telegram.bot)
  
  bot = Bot(token = "706611759:AAG4SM86fmKX9P-ovHLgBI9fUi9UCcxSDuI")
  
  updates <- bot$getUpdates()
  
  chat_id <- updates[[1L]]$from_chat_id()
  
  time <- as.character(now())
  
  bot$sendMessage(chat_id = chat_id, text = glue::glue("An error occured while sending email - {time}"))
}

my_email <- "hr.pchristensen@gmail.com"

smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "hr.pchristensen@gmail.com",
               password = "11euheld")

if (nrow(new) == 0) {
    
    text <- "Ingen nye sure smileys i Aarhusområdet"
    subject <- "Smiley update"
    
    email <- envelope() %>%
      from(my_email) %>%
      to(my_email) %>%
      subject(subject) %>%
      body(text)
    
    tryCatch({
      smtp(email)}, error=function(e) {error_message()
      }
    )
  }
  
  if (nrow(new) >= 1) {
    
    virksomheder = paste(new$navn1,collapse=", ")
    text <- glue::glue("Kære NN\n\nDe seneste sure smileys er givet til følgende virksomheder: {virksomheder}.\n\n De nye datarækker i smiley-registret er vedhæftet i en csv-fil.\n\nMed venlig hilsen\n\nPeer Christensen\n\n")
    
    email <- envelope() %>%
      from(my_email)    %>%
      to(my_email)      %>%
      subject("Seneste smileys") %>%
      body(text) %>%
      attachment(new_filename)
    
    tryCatch({
      smtp(email)}, error=function(e) {error_message()
        
      }
    )
  }

