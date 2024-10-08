rm(list = ls()) # remove all objects
gc() # garbage collection

library(tidyverse)
library(arrow)

setwd("C:/Users/maguf/OneDrive/Documentos/datamining2024")
dataset <- fread("./datasets/competencia_01.csv")

df <- dataset %>% count(foto_mes, clase_ternaria) %>% group_by(foto_mes) %>%  mutate(p = n/sum(n)) %>% arrange(foto_mes, clase_ternaria) %>%  collect()

print (df)