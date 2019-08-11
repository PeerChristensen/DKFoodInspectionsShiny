# Smileydata til Aarhus update
# køres dagligt
# notifikation sendes først via Telegram,
# hvorefter en email kan sendes til modtageren med opdateringer

Sys.setenv("HOME" = "/Users/peerchristensen/Desktop/Projects/Smiley_Data")
setwd("/Users/peerchristensen/Desktop/Projects/Smiley_Data")

# til log-fil
print(date())

# ----- PAKKER ----------

library(tidyverse)
library(readxl)
library(lubridate)
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

if (length(file_dates) >= 32) {
  file_dates_delete <- do.call("c",file_dates) %>%
    .[32:length(file_dates)]
  
  file_dates_delete %>% map(function(x) str_match_all(string=x,pattern=as.character(file_dates_delete))) %>% 
    unlist() %>%
    map(function(x) paste0("smiley_rapport_",x,".csv")) %>%
    unlist() %>% 
    map(function(x) unlink(x))
}

file_dates <- do.call("c",file_dates) %>%
  .[1:2]

# ----- LAV FULD DELTA-FIL ------

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

new <- dplyr::setdiff(df_2,df_1) %>%
  mutate(kontrol_beskrivelse = case_when(seneste_kontrol == 1 ~ "ingen anmærkninger",
                                         seneste_kontrol == 2 ~ "indskærpelse",
                                         seneste_kontrol == 3 ~ "Påbud eller forbud",
                                         seneste_kontrol == 4 ~ "(sur smiley) bødeforlæg, politianmeldelse.."
  ))

new_filename <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/deltas/smiley_delta_",today(),".csv")

if (nrow(new) > 0) {
  write_csv(new, new_filename)
}

# ----- DK UPDATE ------

update <- new %>%
  filter(seneste_kontrol == 4)

update_filnavn <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/delta_sure/smiley_delta_dk_",today(),".csv")

if (nrow(update) > 0) {
  update <- update %>%
    select(navn1,cvrnr,By,URL)
  write_csv(update, update_filnavn)
}

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

if (nrow(update) == 0) {
  
  text <- "Ingen nye sure smileys i dag"
  subject <- "Smiley update: 0 nye"
  
  email <- envelope() %>%
    from(my_email) %>%
    to(my_email) %>%
    #    cc("") %>%
    subject(subject) %>%
    body(text)
  
  tryCatch({
    smtp(email)}, error=function(e) {error_message()
    }
  )
}

if (nrow(update) >= 1) {
  
  virksomheder = paste(update$navn1,collapse=", ")
  text <- glue::glue("Hej!\n\nDe seneste sure smileys er givet til følgende virksomheder: {virksomheder}.\n\n De nye datarækker i smiley-registret er vedhæftet i en csv-fil.\n\nMed venlig hilsen\n\nPeer Christensen\n\n")
  
  email <- envelope() %>%
    from(my_email)    %>%
    to(my_email)      %>%
    subject(glue::glue("Smiley update: {nrow(update)} nye")) %>%
    body(text) %>%
    attachment(update_filnavn)
  
  tryCatch({
    smtp(email)}, error=function(e) {error_message()
      
    }
  )
}

# # ----- AARHUS UPDATE ------
# 
# aarhus <- new %>%
#   filter(str_starts(postnr, "8"),seneste_kontrol >= 3)
# 
# aarhus_filnavn <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/delta_aarhus/smiley_delta_aarhus",today(),".csv")
# 
# if (nrow(aarhus) > 0) {
#   write_csv(aarhus, aarhus_filnavn)
# }
# 
# # error message function
# error_message <- function(x) {
#   
#   library(telegram.bot)
#   
#   bot = Bot(token = "706611759:AAG4SM86fmKX9P-ovHLgBI9fUi9UCcxSDuI")
#   
#   updates <- bot$getUpdates()
#   
#   chat_id <- updates[[1L]]$from_chat_id()
#   
#   time <- as.character(now())
#   
#   bot$sendMessage(chat_id = chat_id, text = glue::glue("An error occured while sending email - {time}"))
# }
# 
# my_email <- "hr.pchristensen@gmail.com"
# 
# smtp <- server(host = "smtp.gmail.com",
#                port = 465,
#                username = "hr.pchristensen@gmail.com",
#                password = "11euheld")
# 
# if (nrow(aarhus) == 0) {
#   
#   text <- "Ingen nye sure smileys i Aarhusområdet"
#   subject <- "Smiley update: 0 nye"
#   
#   email <- envelope() %>%
#     from(my_email) %>%
#     to(my_email) %>%
# #    cc("") %>%
#     subject(subject) %>%
#     body(text)
#   
#   tryCatch({
#     smtp(email)}, error=function(e) {error_message()
#     }
#   )
# }
# 
# if (nrow(aarhus) >= 1) {
#   
#   virksomheder = paste(aarhus$navn1,collapse=", ")
#   text <- glue::glue("Kære NN\n\nDe seneste sure smileys er givet til følgende virksomheder: {virksomheder}.\n\n De nye datarækker i smiley-registret er vedhæftet i en csv-fil.\n\nMed venlig hilsen\n\nPeer Christensen\n\n")
#   
#   email <- envelope() %>%
#     from(my_email)    %>%
#     to(my_email)      %>%
#     subject(glue::glue("Smiley update: {nrow(aarhus)} nye")) %>%
#     body(text) %>%
#     attachment(aarhus_filnavn)
#   
#   tryCatch({
#     smtp(email)}, error=function(e) {error_message()
#       
#     }
#   )
# }

# ----- København + ------

kbh <- new %>%
  filter(  str_starts(postnr, "1") |
           str_starts(postnr, "2") |
           str_starts(postnr, "3"),
           seneste_kontrol == 4) %>%
  select(navn1,cvrnr,By,URL)

kbh_filnavn <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/delta_kbh/smiley_delta_kbh",today(),".csv")

if (nrow(kbh) > 0) {
  write_csv(kbh, kbh_filnavn)
}

# ----- FYN ------

fyn <- new %>%
  filter(  str_starts(postnr, "5"),
           seneste_kontrol >= 3)

fyn_filnavn <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/delta_fyn/smiley_delta_fyn",today(),".csv")

if (nrow(fyn) > 0) {
  write_csv(fyn, fyn_filnavn)
}

# ----- SYD ------

syd <- new %>%
  filter(  str_starts(postnr, "6"),
           seneste_kontrol >= 3)

syd_filnavn <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/delta_syd/smiley_delta_syd",today(),".csv")

if (nrow(syd) > 0) {
  write_csv(syd, syd_filnavn)
}

# ----- NORD ------

nord <- new %>%
  filter(  str_starts(postnr, "9"),
           seneste_kontrol >= 3)

nord_filnavn <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/delta_nord/smiley_delta_nord",today(),".csv")

if (nrow(nord) > 0) {
  write_csv(nord, nord_filnavn)
}

#library(tidyverse)
#df <- read_csv(paste0("deltas/",list.files("deltas")[length(list.files("deltas"))]))
# df %>% filter(seneste_kontrol == 4) %>% select(By,navn1)
