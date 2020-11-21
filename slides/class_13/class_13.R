library("tidyverse")
library("broom")
library("viridis")


setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2020_2_data_analysis_r/repo/slides/class_12/")

# leer archivo csv
covid_data <- read_delim("covid_data.csv", delim=";")

covid_data <- covid_data %>% filter(date==as.Date("2020-11-17")) 
covid_data  %>% glimpse()



# Loop

## Análisis de datos Covid-19 usando loops + almacenamiento resultados via `assign()`


for (dv in c("total_cases_per_million", "total_deaths_per_million")) {
  for (iv in  c("population_density", "median_age", "gdp_per_capita")) {
    
    #ajusta modelo
    y <- covid_data[,dv] %>% as.matrix()
    x <- covid_data[,iv] %>% as.matrix()
    mimodelo <- lm(y ~ x)
    namemodelo <- paste("lm_",dv,"_",iv) 
    assign(namemodelo,mimodelo ) 
    
    #r2 
    mir2 <- summary(mimodelo)
    namer2 <- paste("r2_",dv,"_",iv) 
    assign(namer2,mir2)
    
  }
}



# Purrr

fit_lm <- covid_data %>% group_by(continent,dv,iv) %>% nest() %>%
  mutate(fit = map(data, ~ lm(y ~ x, data = .x))) %>% 
  mutate(coefs = map(fit, tidy)) %>% 
  dplyr::select(continent,dv,iv,coefs) %>% 
  unnest()


plot <- fit_lm %>% filter(term=="x") %>%  
  ggplot(aes(x=continent, y=estimate, fill=continent)) +  
  geom_col() + facet_wrap(dv ~ iv, scales="free_y") +
  scale_fill_viridis_d() +
  theme_bw()
