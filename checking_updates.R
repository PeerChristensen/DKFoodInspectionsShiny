# script for checking updates

library(tidyverse)
df <- read_csv(paste0("deltas/",list.files("deltas")[length(list.files("deltas"))]))
df %>% filter(seneste_kontrol == 4) %>% select(By,navn1,URL)
