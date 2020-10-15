## readr: cargar archivos .cvs

library("readr") 

setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/gentle-ggplot2/data/")

# leer archivo csv
data_casen_csv <- read_csv("sample_casen2017.csv")

head(as.data.frame(data_casen_csv),4)

## readr: cargar archivos .dta (Stata)

library("haven") 

setwd(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/gentle-ggplot2/data/"
)

# leer archivo dta 
data_casen_dta <- read_dta("sample_casen2017.dta", encoding = "latin1", .name_repair = "minimal")


## Crear un tibble

library("tibble")

mytibble <- 
  tibble(
    x=rep(c("A","B","C"),8),
    y=sample(1:10,size=24, replace = T), 
    z=factor(sample(letters,size=24))
  )

print(mytibble)


## Transformar un data frame en tibble

as.data.frame(data_casen_csv)
as_tibble(data_casen_csv)


## Pipes

round(mean(data_casen_csv$ytotcor[(data_casen_csv$esc <= 12 & data_casen_csv$sexo == 2)], 
           na.rm = TRUE),0)

sub_muestra <- data_casen_csv$ytotcor[
  (data_casen_csv$esc <= 12 &
     data_casen_csv$sexo == 2)]

my_mean <- mean(sub_muestra, 
                na.rm=TRUE)

round(my_mean,0)

# pipes
data_casen_csv %>%
  filter(esc <= 12 & sexo == 2) %>%
  with(mean(ytotcor, na.rm =T)) %>%
  round(0)


## arrange: ordenación de datos

data_casen_csv %>% arrange(edad)
data_casen_csv %>% arrange(desc(edad))
data_casen_csv %>% arrange(sexo,edad)
data_casen_csv %>% arrange(edad,desc(sexo),yautcor)

## select: selección de variables

data_casen_csv %>% select(sexo,edad,educ)
data_casen_csv %>% select(!c(sexo,edad,educ))
data_casen_csv %>% select(1:5,8)
data_casen_csv %>% select(starts_with("y"))
data_casen_csv %>% select(ends_with("a"))
data_casen_csv %>% select(contains("cor"))
data_casen_csv %>% select((num_range("x", 10:15)))

## filter: selección de variables

data_casen_csv %>% filter(sexo==2)
data_casen_csv %>% filter(region==7)
data_casen_csv %>% filter(region!=7)
data_casen_csv %>% filter(region>7)
data_casen_csv %>% filter(region<=7)
data_casen_csv %>% filter(sexo==2 & edad>=18)
data_casen_csv %>% filter(sexo==2 & edad>=18 & (region==2 | region==6) )

data_casen_csv %>% 
  filter(sexo==2 | region==13) %>% 
  select(sexo, region) %>% head()

data_casen_csv %>% 
  filter(xor(sexo==2, region==13)) %>% 
  select(sexo, region) %>% head()


