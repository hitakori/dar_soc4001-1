
# Seleccionar set de datos de interés

vars <- c("cntry","gndr","yrbrn","eisced")

essdata_sub <- essdata[,vars]

dim(essdata_sub)

head(essdata_sub)




# filtra filas que cumplen condición lógica 

attach(essdata_sub)

essdata_sub <- essdata_sub[cntry!="AT" & yrbrn>1990, ]
dim(essdata_sub)
  
# crea nueva variable

age <- 2019 - yrbrn

essdata_sub$age <- 2019 - yrbrn


# recodifica variable existente

essdata_sub$gndr_string = ifelse(gndr == 1, "Male", ifelse(gndr == 2, "Female", NA))

sum(is.na(essdata_sub$gndr_string))
sum(is.na(essdata_sub$gndr))

detach(essdata_sub)
