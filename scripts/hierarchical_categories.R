# load the packages ----------------------------------------------------------
library(here)
library(tidyverse)
library(rstatix)


# data -----------------------------------------------------------------------
library(readxl)
meldata <- read_excel(here("data", "meldata.xlsx"), col_names=TRUE)
glimpse(meldata)


meldata <- meldata %>%
  convert_as_factor(status, sex, ulcer) %>% 
  select(where(is.factor)) %>% 
  relocate(ulcer, sex, status)


# create the plot ----------------------------------------------------------
library(plotrix)

# set the colors
colrs<-list(c("#000000","#dddd00"),
             c("pink","lightblue"),
             c("gold", "grey70"))

# the hierarchical categories plot
sizetree(meldata, col=colrs, right=1)
