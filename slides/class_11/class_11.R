# Limpiar pantalla y remover objetos existentes
cat("\014") 
rm(list = ls())

## Bases de datos ordenadas ("tidy") 

library("tidyverse")
library("ineq")
library("ggsci")
library("viridis")

setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2020_2_data_analysis_r/repo/slides/class_5/")

# leer archivo csv
data_casen_csv <- read_csv("sample_casen2017.csv")
data_casen_csv %>% glimpse()

data_ineq <- data_casen_csv %>% 
  group_by(comuna,educ,sexo) %>% 
  summarise(gini_ytot=Gini(ytotcor,na.rm = T), gini_yaut=Gini(yautcor,na.rm = T), media_yaut=mean(yautcor,na.rm = T)) %>%
  mutate(ratio=gini_ytot/gini_yaut) %>%
  mutate(educ = if_else(educ==99,NA_real_,educ))

# Construir gráfico en layers

# paso 1:

data_ineq %>% ggplot()

#o

ggplot(data=data_ineq) 


# paso 2

# color continuo
data_ineq %>% ggplot(aes(x=media_yaut, y=gini_yaut)) + geom_point()
                     
g <- data_ineq %>% ggplot(aes(x=log(media_yaut), y=gini_yaut, colour=educ)) +
  geom_point(size=2, alpha=0.4) +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 10) +
  ylim(0,0.7) +
  theme_bw() +
  facet_grid( sexo ~ .) +
  labs(x="Log median ingreso autónomo indiv.",  y="Gini Ingreso autonomo indiv.", color = "Educación") +
  scale_color_viridis(option="magma") 

g
data_ineq %>% ggplot(aes(x=log(media_yaut), y=gini_yaut, colour=educ)) +
  geom_point(size=2, alpha=0.4) +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 10) +
  ylim(0,0.7) +
  theme_bw() +
  facet_grid( . ~ sexo) +
  labs(x="Log median ingreso autónomo indiv.",  y="Gini Ingreso autonomo indiv.", color = "Educación") +
  scale_color_viridis(option="magma") 


# RETOMAR: jitter + transparencia

data_casen_csv %>% ggplot(aes(x=esc, y=yautcor, colour=sexo)) +
  geom_point(size=2, alpha=0.4) +
  theme_bw() +
  scale_color_viridis(option="magma") 

