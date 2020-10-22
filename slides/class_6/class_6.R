# Carga datos

library("readr") 

setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2020_2_data_analysis_r/repo/slides/class_5/")

# leer archivo csv
data_casen_csv <- read_csv("sample_casen2017.csv")


# Seleccion subconjunto de datos

library("tidyverse")
data_casen_csv %>% select(sexo,edad,ytotcor)

# Creación de nuevas variables

data_casen_csv %>% select(sexo,edad,ytotcor) %>% mutate(anno = 2017)

data_casen_csv %>% select(sexo,edad,ytotcor) %>% mutate(ytotcor_mm = log(ytotcor/1000) + 1)


# mutate, if_else: creación de datos

data_casen_csv %>% select(sexo,edad,ytotcor) %>% 
  mutate(sexo = if_else(sexo==1,1,0)) 

data_casen_csv %>% select(sexo,edad,ytotcor) %>% 
  mutate(sexo = if_else(sexo==1,sexo,0))


## mutate, cases_when: creación de datos


data_casen_csv %>% select(sexo,edad,ytotcor) %>% 
  mutate(edad_cat = case_when(edad <= 18 ~ 1, 
                              edad >18 & edad<=65 ~ 2, 
                              edad > 65  ~ 3) 
  )


data_casen_csv %>% select(sexo,edad,ytotcor) %>% 
  mutate(edad_cat = case_when(edad <= 18 ~ 1,
                              edad >18 & edad<=65 ~ 2, 
                              edad > 65  ~ 3, 
                              TRUE ~ edad)
  )


## group_by: operaciones agrupadas.


data_casen_csv %>% 
  group_by(region) %>% 
  mutate(n_region = n(), mean_ytotcor_region = mean(ytotcor, na.rm = T)) %>% 
  ungroup() %>% 
  select(region,sexo,edad, ytotcor,n_region, mean_ytotcor_region) 


data_casen_csv %>% 
  group_by(region, sexo) %>% 
  mutate(n_region = n(), mean_ytotcor_region = mean(ytotcor, na.rm = T)) %>%
  ungroup() %>% 
  select(region,sexo,edad, ytotcor,n_region, mean_ytotcor_region)