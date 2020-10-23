# Carga datos
library("readr") 

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2020_2_data_analysis_r/repo/slides/class_5/")

# leer archivo csv
data_casen_csv <- read_csv("sample_casen2017.csv")

## summarise: extrayendo información de los datos

library(tidyverse)
data_casen_csv %>% 
  summarise(mean_ytotcor_region = mean(ytotcor, na.rm = T)) 


data_casen_csv %>% 
  summarise(mean_ytotcor_region = mean(ytotcor, na.rm = T), 
            median_ytotcor_region = median(ytotcor, na.rm = T)) / 1000


#Quantile:

data_casen_csv %>% 
  summarise(q20=quantile(ytotcor, probs=0.2, na.rm = T))


#3er caso:
data_casen_csv %>% 
  summarise(tercer=nth(ytotcor, n=3))

#Cantidad de valores distintos
data_casen_csv %>% 
  summarise(distintos=n_distinct(ytotcor))


## summarise across() variables

data_casen_csv %>% 
  summarise(across( c("sexo","edad"), mean))


data_casen_csv %>% 
  summarise(across(starts_with("y"), mean))


data_casen_csv %>% 
  summarise(across(starts_with("y"), ~mean(.x, na.rm = TRUE) ))


## summarise una variable con una lista de funciones

data_casen_csv %>% 
  summarise(across(edad, list(media = mean, mediana = median)))

data_casen_csv %>% 
  summarise(across(ytrabajocor, list(media =  ~mean(.x, na.rm = TRUE), mediana = ~median(.x, na.rm = TRUE))))


## summarise muchas variable con una lista de funciones

data_casen_csv %>% 
  summarise(across(starts_with("y"), list(media = ~mean(.x, na.rm = TRUE), mediana= ~median(.x, na.rm = TRUE)) ))


## summarise by_group(): resumiendo datos agupados


data_casen_csv %>% 
  group_by(region, sexo) %>% 
  summarise(mean_ytotcor_region = mean(ytotcor, na.rm = T), 
            median_ytotcor_region = median(ytotcor, na.rm = T))


# Combinando herramientas

data_casen_csv %>% 
  group_by(region, sexo) %>% 
  summarise(across(ytrabajocor, list(media =  ~mean(.x, na.rm = TRUE), mediana = ~median(.x, na.rm = TRUE))))


## join: juntar bases de datos


data_a <- data_casen_csv %>% filter(region <2 | region>=15 ) %>%
  group_by(region) %>% 
  summarise(across(edad, mean)); data_a

data_b <- data_casen_csv %>% filter(region <3 | region>=16 ) %>%
  group_by(region) %>% 
  summarise(across(c(edad,yautcorh), mean)); data_b


#`inner_join()`

data_a %>% inner_join(data_b, by="region")

data_a %>% inner_join(data_b %>% select(!edad), by="region")

data_a %>% select(!edad) %>% inner_join(data_b, by="region")

#`left_join()`

data_a %>% left_join(data_b  %>% select(!edad), by="region")

#`right_join()`
data_a  %>% right_join(data_b %>% select(!edad), by="region")

#`full_join()`
data_a  %>% full_join(data_b %>% select(!edad), by="region")



## join: juntar bases de datos por más de una llave

data_casen_csv %>% 
  select(region,sexo,yautcorh)

gini_regsex <- data_casen_csv %>% 
  group_by(region,sexo) %>% 
  summarise(gini = Gini(yautcorh, na.rm = T )); gini_regsex


data_casen_csv %>% 
  left_join(gini_regsex, by=c("region","sexo")) %>% 
  select(region,sexo,yautcorh, gini)