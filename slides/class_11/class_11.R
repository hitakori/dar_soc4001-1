# Limpiar pantalla y remover objetos existentes
cat("\014") 
rm(list = ls())

## Bases de datos ordenadas ("tidy") 

library("tidyverse")
library("ineq")


setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2020_2_data_analysis_r/repo/slides/class_5/")

# leer archivo csv
data_casen_csv <- read_csv("sample_casen2017.csv")
data_casen_csv %>% glimpse()

data_ineq <- data_casen_csv %>% 
  group_by(comuna,sexo) %>% 
  summarise(gini_ytot=Gini(ytotcor,na.rm = T), gini_yaut=Gini(yautcor,na.rm = T))


# Construir gráfico en layers

data_ineq %>%
  ggplot(
    aes(x=gini_yaut, y=gini_ytot, group = factor(sexo),  colour = factor(sexo), shape = factor(sexo))) +
   geom_point()  +
   geom_smooth(se=F) + 
   labs(x="Gini Ingreso autónomo indiv.",  y="Gini Ingreso total indiv.",
        title = "Redistribución de ingresos por región", shape = "Sexo", color = "Sexo") 