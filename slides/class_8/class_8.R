# Limpiar pantalla y remover objetos existentes
cat("\014") 
rm(list = ls())

# Carga paquetes
library("tidyverse")


# Establecer directory de trabajo


dirdata <- setwd("/Users/Mauricio/Desktop/ENE/")
getwd()

# Importar datos Encuesta Nacional de Empleo

#read_csv("ENE201905.csv") 

data_ene <- read_delim("ENE201901.csv", delim = ";") %>%  
  dplyr::select(region,id_identificacion,idrph,id_directorio,ano_encuesta,mes_encuesta,estrato,hogar,edad,sexo,parentesco,curso,nivel,a1,a3,b3,c1,starts_with("c2"))

read_delim("ENE201901.csv", delim = ";") %>% glimpse()
data_ene %>% glimpse()

# juntas bases de datos

for (i in 2:8) {

  newfile <- paste0("read_delim(","'ENE20190",i,".csv', delim= ';' )"); print(newfile)
  newdata <- eval(parse( text = newfile )) 
  
  newdata <- newdata  %>% 
    dplyr::select(region,ano_encuesta,mes_encuesta,estrato,hogar,edad,sexo,parentesco,curso,nivel,a1,a3,b3,starts_with("c2"))
  
  data_ene <- data_ene %>% bind_rows(newdata) 
}

# explorar 
data_ene %>% group_by(ano_encuesta,mes_encuesta) %>% summarise(n())

# renombrar variables
data_ene <- data_ene %>% rename(trab_sempas = a1,  trab_pagado = a3,  trab_asalariado = b3, jornada_comp = c1)


# recode
rec <- function(x) {
  case_when(x==1 ~ 1, x==2~ 0)
}

data_ene <-  data_ene %>% mutate(across(trab_sempas:jornada_comp,~ rec(.x) ))


# crear nuevas variables

data_ene %>% mutate(horas_totales = c2_1_1*c2_1_2 + c2_2_1*c2_2_2)


data_ene %>% rowwise() %>% mutate(horas_totales = sum(c2_1_1*c2_1_2,c2_2_1*c2_2_2, na.rm = T))


data_ene <- data_ene %>% ungroup() %>% mutate(across(starts_with("c2"), ~ if_else(.x==999, NA_real_,.x) )) %>%
  rowwise() %>% mutate(horas_totales = sum(c2_1_1*c2_1_2,c2_2_1*c2_2_2, na.rm = T)) %>% 
  dplyr::select(-starts_with("c")) 

  

# filtra jefe de hogar y conyuge, 18 - 65 años, year=2019 

data_ene <- data_ene %>%  ungroup() %>% 
  filter(ano_encuesta==2019, ( parentesco==1 | parentesco==2), edad>=18, edad<=65)


# summarise
 
tabla_1 <- data_ene %>% group_by(mes_encuesta,sexo,nivel) %>% 
  summarise(media_horas_totales = mean(horas_totales, na.rm = T)) 

tabla_1 %>% 
  ggplot(aes(x=mes_encuesta,y=media_horas_totales, colour=factor(nivel), group=factor(nivel))) + 
  geom_line() + facet_grid( ~ sexo)


# colapsar datos
  
data_ene_agg <- data_ene %>%  
  mutate(across( nivel, ~ case_when(.x==999 ~ NA_real_ , .x==99 ~ NA_real_ , TRUE ~ .x) )) %>%
  group_by(region,ano_encuesta,mes_encuesta,estrato,hogar) %>% 
   summarise(across(c(edad,nivel,starts_with("trab"),jornada_comp,horas_totales), ~ mean(.x, na.rm = T) ))
   
data_ene_agg %>% 
  ggplot(aes(x=nivel, y= horas_totales)) + 
  geom_point(alpha=0.1)


# juntar datos


dirdata <- setwd("/Users/Mauricio/Desktop/")
covid_region <- read_csv("covid_region.csv")

regs <- unique(covid_region$Region)
covid_region <- covid_region %>% mutate(region = case_when(
  Region == regs[1]  ~ 15,
  Region == regs[2]  ~ 1,
  Region == regs[3]  ~ 2,
  Region == regs[4]  ~ 3,
  Region == regs[5]  ~ 4,
  Region == regs[6]  ~ 5,
  Region == regs[7]  ~ 13,
  Region == regs[8]  ~ 6,
  Region == regs[9]  ~ 7,
  Region == regs[10] ~ 16,
  Region == regs[11] ~ 8,
  Region == regs[12] ~ 9,
  Region == regs[13] ~ 14,
  Region == regs[14] ~ 10,
  Region == regs[15] ~ 11,
  Region == regs[16] ~ 12
  )
)

covid_region <-  covid_region %>% rename(casos_tot_acum = `Casos totales acumulados`)


data_ene %>% left_join(covid_region %>%  dplyr::select(region,casos_tot_acum), by="region")


data_ene_agg %>% full_join(covid_region, by="region")
  


