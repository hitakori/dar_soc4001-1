# Limpiar pantalla y remover objetos existentes
cat("\014") 
rm(list = ls())

library("tidyverse")
library("carData")
library("janitor")

data(WeightLoss)
wl <- WeightLoss %>% select(group,starts_with("w")) %>% as_tibble(); rm(WeightLoss); wl


wl %>% 
  pivot_longer(
    cols = starts_with("w")) 


data(WeightLoss)
wl <- WeightLoss %>% select(group,starts_with("w")) %>% as_tibble(); rm(WeightLoss); wl

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
    names_prefix="wl") 

wl %>% pivot_longer(cols = -c(id,group),     
                    names_to = "outcome_week", 
                    values_to = "score") 


wl %>% pivot_longer(cols = -c(id,group),     
                    names_to = "outcome_week",
                    values_to = "score")


wl_long <- wl %>% pivot_longer(cols = -c(id,group),     
                               names_to = "outcome_week",
                               values_to = "score") %>%
  separate(outcome_week, into = c("outcome", "week"), sep = 2); wl_long


wl_long %>% 
  pivot_wider(names_from = outcome, values_from = score)


wl_long %>% mutate(error = rnorm(n()))


wl_long %>% mutate(error = rnorm(n())) %>%
  pivot_wider(names_from = outcome, values_from = c(score, error))  


wl_long %>% mutate(error = rnorm(n())) %>%
  pivot_wider(names_from = c(outcome,week), values_from = c(score, error))  



data_mpd_messy <- read_delim("mpd2020.csv", delim = ";")

data_mpd <- data_mpd_messy %>% select(X1,starts_with("GDP")) %>% row_to_names(row_number = 1) %>%
  rename(year=Region) %>% 
  filter(year!="Year") %>%
  pivot_longer(-year, names_to= "region", values_to="population"); data_mpd


data_mpd <- data_mpd %>% separate(region, into = c("zone", "continent"), sep = " ", extra="merge")
data_mpd 


data_mpd <- data_mpd %>% unite(col = "region", c("zone","continent"), sep = "-")
data_mpd 


data_mpd_messy <- read_delim("mpd2020.csv", delim = ";")
data_mpd_messy


data_mpd_messy %>% select(X1,starts_with("GDP")) %>% row_to_names(row_number = 1) %>%
  rename(year=Region) %>% 
  filter(year!="Year") 



data_mpd_gpd <- data_mpd_messy %>% select(X1,starts_with("GDP")) %>% row_to_names(row_number = 1) %>%
  rename(year=Region) %>%
  filter(year!="Year") %>%
  pivot_longer(-year, names_to= "region", values_to="dgp"); data_mpd_gpd  data_mpd_gpd 



data_mpd_pop <- data_mpd_messy %>% select(X1,starts_with("Population")) %>% row_to_names(row_number = 1) %>%
  rename(year=Region) %>%
  filter(year!="Year") %>%
  pivot_longer(-year, names_to= "region", values_to="population"); data_mpd_pop



data_mpd <- data_mpd_gpd %>% left_join(data_mpd_pop, by=c("year","region"))
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