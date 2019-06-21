library(tidyverse)
library(readxl)
library(RCurl)
library(lubridate)
library(sqldf)
library(gmailr)

url <- "https://www.foedevarestyrelsen.dk/_layouts/15/sdata/smileystatus.zip"

destfile <- paste0("smiley_rapport_",today())

download.file(url = url, destfile = destfile,method="libcurl")

unzip(destfile)

destfile2 <- paste0(destfile,".xls")

file.rename(from = "SmileyStatus.xls", to = destfile2)

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

files_compare <- files %>% 
  map(function(x) str_match_all(string=x,pattern=as.character(file_dates))) %>% 
  unlist() %>%
  map(function(x) paste0("smiley_rapport_",x,".xls")) %>%
  unlist()

df_1 <- read_excel(files_compare[1])
df_2 <- read_excel(files_compare[2])

new <- sqldf('SELECT * FROM df_1 EXCEPT SELECT * FROM df_2')

new_filename <- paste0("deltas/smiley_delta_",today(),".csv")

write_csv(new, new_filename)

#check

#df <- read_csv("deltas/smiley_delta_2019-06-21.csv")

my_email <- "hr.pchristensen@gmail.com"

new_controls <- new %>% select(navn1,adresse1,seneste_kontrol)

new_html <- tableHTML(new_controls)

html_body <- "this is a test"
  
mime()              %>%
  from(my_email)    %>%
  to(my_email)      %>%
  subject("From R") %>%
  text_body(html_body) %>%
  attach_part(html_body) %>%
  attach_file(new_filename) %>%
  send_message()

