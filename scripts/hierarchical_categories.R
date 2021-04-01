library(here)
library(tidyverse)
library(rstatix)

library(readxl)
meldata <- read_excel(here("data", "meldata.xlsx"), col_names=TRUE)
glimpse(meldata)


meldata <- meldata %>%
  convert_as_factor(status, sex, ulcer) %>% 
  select(where(is.factor)) %>% 
  relocate(ulcer, sex, status)


library(plotrix)
shecol<-list(c("#000000","#dddd00"),
             c("pink","lightblue"),
             c("gold", "grey70"))


sizetree(meldata, col=shecol, right=1)
