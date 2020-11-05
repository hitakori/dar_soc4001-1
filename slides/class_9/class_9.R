# Limpiar pantalla y remover objetos existentes
cat("\014") 
rm(list = ls())

library("tidyverse")
library("carData")
library("janitor")

data(WeightLoss)
wl <- WeightLoss %>% dplyr::select(group,starts_with("w")) %>% as_tibble(); rm(WeightLoss); wl


wl %>% 
  pivot_longer(
    cols = starts_with("w")) 


wl %>% 
  pivot_longer(
    cols = starts_with("w"), 
    names_to="week", 
    values_to= "lbs_lost") 

wl %>% 
  pivot_longer(
    cols = starts_with("w"), 
    names_to="week", 
    values_to= "lbs_lost",
    names_prefix="wl")  %>%
    mutate(week = as.integer(week))



data(WeightLoss)
wl <- WeightLoss  %>% as_tibble(); rm(WeightLoss); wl

 
wl %>% mutate(id = 1:n()) %>%
  pivot_longer(cols = -c(id,group),     
                    names_to = "outcome_week", 
                    values_to = "score") 


wl_long <- wl %>% mutate(id = 1:n()) %>% 
      pivot_longer(cols = -c(id,group),     
                               names_to = "outcome_week",
                               values_to = "score") %>%
  separate(outcome_week, into = c("outcome", "week"), sep = 2); wl_long


wl_long %>% 
  pivot_wider(names_from = outcome, values_from = score)



wl_long %>% mutate(error = rnorm(n())) %>%
  pivot_wider(names_from = outcome, values_from = c(score, error))  


wl_long %>% mutate(error = rnorm(n())) %>%
  pivot_wider(names_from = c(outcome,week), values_from = c(score, error))  


##############################

setwd("/Users/Mauricio/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2020_2_data_analysis_r/repo/slides/class_9")

data_mpd_messy <- read_delim("mpd2020.csv", delim = ";")


data_mpd_gdp <- data_mpd_messy %>% dplyr::select(X1,starts_with("GDP")) %>% row_to_names(row_number = 1) %>%
  rename(year=Region) %>% 
  filter(year!="Year") %>%
  pivot_longer(cols = -year, names_to= "region", values_to="gdp"); data_mpd_gdp


data_mpd_pop <- data_mpd_messy %>% dplyr::select(X1,starts_with("Pop")) %>% row_to_names(row_number = 1) %>%
  rename(year=Region) %>% 
  filter(year!="Year") %>%
  pivot_longer(cols = -year, names_to= "region", values_to="population"); data_mpd_pop



data_mpd <- data_mpd_gdp %>% left_join(data_mpd_pop, by=c("year","region"))
data_mpd 



setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2020_2_data_analysis_r/repo/slides/class_5/")

# leer archivo csv
data_casen_csv <- read_csv("sample_casen2017.csv")


data_casen_csv %>% 
  summarise(across(starts_with("y"), list(media = ~mean(.x, na.rm = TRUE), mediana= ~median(.x, na.rm = TRUE)) ))


data_casen_csv %>% 
  summarise(across(starts_with("y"), list(media = ~mean(.x, na.rm = TRUE), mediana= ~median(.x, na.rm = TRUE)) )) %>%
  pivot_longer( 
    everything(), 
    names_to="outcome_stat", 
    values_to="value")  


data_casen_csv %>% 
  summarise(across(starts_with("y"), list(media = ~mean(.x, na.rm = TRUE), mediana= ~median(.x, na.rm = TRUE)) )) %>%
  pivot_longer(
    everything(), 
    names_to="outcome_stat",
    values_to="value"
  ) %>%
  separate(outcome_stat, sep="_", into = c("outcome","stat")) 


data_casen_csv %>% 
  summarise(across(starts_with("y"), list(media = ~mean(.x, na.rm = TRUE), mediana= ~median(.x, na.rm = TRUE)) )) %>%
  pivot_longer(
    everything(), 
    names_to="outcome_stat",
    values_to="value"
  ) %>%
  separate(outcome_stat, sep="_", into = c("outcome","stat") ) %>%
  pivot_wider(names_from = "stat", values_from = "value") 