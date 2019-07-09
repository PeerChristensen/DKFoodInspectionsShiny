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
library(tableHTML)

# ----- DOWNLOAD FIL -----

url <- "https://www.foedevarestyrelsen.dk/_layouts/15/sdata/smileystatus.zip"

destfile <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_data/smiley_rapport_",today())

download.file(url = url, destfile = destfile, method="libcurl")

unzip(destfile)

destfile2 <- paste0(destfile,".xls")

file.rename(from = "SmileyStatus.xls", to = destfile2)

# ----- FORBERED FILER -----

files <- list.files(pattern = ".xls") %>%
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

df_1 <- read_excel(files_compare[1]) #new
df_2 <- read_excel(files_compare[2]) #old

#new <- sqldf('SELECT * FROM df_2 EXCEPT SELECT * FROM df_1') %>%
new <- dplyr::setdiff(df_2,df_1) %>%
  mutate(kontrol_beskrivelse = case_when(seneste_kontrol == 1 ~ "ingen anmærkninger",
                                         seneste_kontrol == 2 ~ "indskærpelse",
                                         seneste_kontrol == 3 ~ "Påbud eller forbud",
                                         seneste_kontrol == 4 ~ "(sur smiley) bødeforlæg, politianmeldelse.."
  )) %>%
  filter(str_starts(postnr, "8"),seneste_kontrol >= 3)

new_filename <- paste0("/Users/peerchristensen/Desktop/Projects/Smiley_Data/deltas/smiley_delta_",today(),".csv")

if (nrow(new) > 0) {
  write_csv(new, new_filename)
}

#new %>% 
#  as_tibble() %>%
#  select(navn1,cvrnr, adresse1, By, seneste_kontrol, seneste_kontrol_dato, naestseneste_kontrol)

# ------ SEND RESULTATER -----

my_email <- "hr.pchristensen@gmail.com"

smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "hr.pchristensen@gmail.com",
               password = "")

if (nrow(new) == 0) {
  
  text <- "Ingen nye sure smileys i Aarhusområdet"
  subject <- "Smiley update"
  
  email <- envelope() %>%
    from(my_email) %>%
    to(my_email) %>%
    subject(subject) %>%
    body(text)
  
  smtp(email)
  
}

#html not working with emayili
# if (nrow(new) > 0 & nrow(new) < 11) {
#   
#   new_controls <- new %>% 
#     select(navn1,cvrnr, adresse1, By, seneste_kontrol, seneste_kontrol_dato, 
#            naestseneste_kontrol,kontrol_beskrivelse) %>%
#     mutate(seneste_kontrol_dato = as.Date(seneste_kontrol_dato),
#            naestseneste_kontrol = naestseneste_kontrol)
#   
#   new_html <- tableHTML(new_controls,rownames = F) %>%
#     add_theme('rshiny-blue')
#   
#   text1 <- "<p>Kære NN</p>I tabellen nedenfor ses hvilke virksomheder der figurerer i de seneste fødevarkontroller"
#   text2 <- "Såfremt det ønskes, kan en fil med de fulde data om disse virksomheder fremsendes."
#   
#   email <- envelope() %>%
#     from(my_email)    %>%
#     to(my_email)      %>%
#     subject("Seneste smileys") %>%
#     body(glue::glue("<p>{text1}</p> 
#                        <p>{new_html}</p> 
#                        <p>{text2}</p>"))
#     
#     smtp(email)
# }

if (nrow(new) >= 1) {
  
  virksomheder = paste(new$navn1,collapse=", ")
  text <- glue::glue("Kære NN\n\nDe seneste sure smileys er givet til følgende virksomheder: {virksomheder}.\n\n De nye datarækker i smiley-registret er vedhæftet i en csv-fil.\n\nMed venlig hilsen\n\nPeer Christensen\n\n")
  
  email <- envelope() %>%
    from(my_email)    %>%
    to(my_email)      %>%
    subject("Seneste smileys") %>%
    body(text) %>%
    attachment(new_filename)
  
  smtp(email)
}
