# Establecer directory de trabajo

getwd()
setwd("/Users/Mauricio/Desktop")

# Importar datos Casen 2017

install.packages("readstata13")
library("readstata13")

?readstata13
??readstata13
data_casen <-  read.dta13("Casen 2017.dta", convert.factors = TRUE)

# Visualizar datos

data_casen

dim(data_casen)
head(data_casen)
tail(data_casen)
attributes(data_casen)
str(data_casen)
summary(data_casen)


# Seleccionar set de datos de interés


vars <- c("comuna", "sexo", "edad", "esc", "educ" ,"yautcor", "ytotcor", "pobreza")

subdata_casen <- data_casen[,vars]

dim(subdata_casen)
head(subdata_casen)
str(subdata_casen)

# Summary

summary(subdata_casen)


# Transformar variables


subdata_casen$yautcor <- subdata_casen$yautcor/1000
subdata_casen$ytotcor <- subdata_casen$ytotcor/1000

# Crea variable nueva

subdata_casen$pobre <- 1
subdata_casen$pobre[subdata_casen$pobreza=="No pobres"] <- 0
subdata_casen$pobre[is.na(subdata_casen$pobreza)] <- NA


# tablas

table(subdata_casen$pobreza)
table(subdata_casen$pobre)
table(subdata_casen$pobreza,subdata_casen$pobre)

prop.table(table(subdata_casen$pobreza))

# lo mismo que arriba pero guardando el objecto previamente
mitable <- table(subdata_casen$pobreza)
prop.table(mitable)

# tabla con proporciones

pov_educ <- table(subdata_casen$educ,subdata_casen$pobreza)

rows <- nrow(pov_educ)
pov_educ <- pov_educ[-rows,]

prop.table(pov_educ)
prop.table(pov_educ, margin=1)
prop.table(pov_educ, margin=2)


## Funciones básicas para vectores y variables

sum(subdata_casen$yautcor, na.rm = TRUE)   # suma
mean(subdata_casen$yautcor, na.rm = TRUE)   # promedio
sd(subdata_casen$yautcor)     # desviación estándar
max(subdata_casen$yautcor,na.rm = TRUE)    # máximo
which.max(subdata_casen$yautcor) # posición de valor máximo 
min(subdata_casen$yautcor)    # mínimo
which.min(subdata_casen$yautcor) # posición de valor mínimo 
rank(subdata_casen$yautcor)   # ranking de valores
median(subdata_casen$yautcor) # mediana
range(subdata_casen$yautcor, na.rm = TRUE)  # rango
rev(subdata_casen$yautcor)    # revertir elementos
unique(subdata_casen$yautcor) # lista de elementos únicos
length(subdata_casen$yautcor) # largo


# Quantiles

quantile(subdata_casen$yautcor, p=c(.2,.4,.6,.8), na.rm = TRUE)
quantile(subdata_casen$yautcor, p=seq(0,1,by=0.1), na.rm = TRUE)
quantile(subdata_casen$yautcor, p=seq(0,1,by=0.05), na.rm = TRUE)
quantile(subdata_casen$yautcor, p=seq(0,1,by=0.01), na.rm = TRUE)



# Correlaciones 

cor(subdata_casen$yautcor,subdata_casen$esc, use = "complete.obs")

cor(subdata_casen[,c("yautcor","ytotcor","esc")], use = "complete.obs")


# Modelo de regresión lineal

mi_reg <- lm(yautcor ~  sexo + esc + I(esc^2), data = subdata_casen)

mi_reg <- lm(subdata_casen$yautcor ~  subdata_casen$sexo + subdata_casen$esc + I(subdata_casen$esc^2))


lm(yautcor*1000 ~  sexo + esc + I(esc^2), data = subdata_casen)

mi_reg$coefficients
mi_reg$terms
mi_reg$residuals


sum_mi_reg <- summary(mi_reg)
sum_mi_reg$sigma

sqrt(sum_mi_reg$sigma)
sqrt(784.4) 


# Funciones de paquetes

install.packages("ineq")
library("ineq")
?ineq
??ineq

Gini(subdata_casen$yautcor, corr = TRUE, na.rm = TRUE)
=======

# Funciones propias

quantile(subdata_casen$yautcor, p=c(.1,.9), na.rm = TRUE)

ratio9010 <- function(x){
  qq <- quantile(x, p=c(.1,.9), na.rm = TRUE)
  ratio <- qq[2]/qq[1]
  names(ratio) <- "r9010"
  return(ratio)
} 

ratio9010(subdata_casen$yautcor)
ratio9010(subdata_casen$ytotcor)
ratio9010(subdata_casen$esc)


# Filtrar casos

ratio9010(subdata_casen$yautcor[subdata_casen$comuna=="Iquique"])
ratio9010(subdata_casen$yautcor[subdata_casen$comuna=="Vitacura"])
ratio9010(subdata_casen$yautcor[subdata_casen$comuna=="Santiago"])
ratio9010(subdata_casen$yautcor[subdata_casen$comuna=="Lo Barnechea"])
ratio9010(subdata_casen$yautcor[subdata_casen$comuna=="Aysén"])
ratio9010(subdata_casen$yautcor[subdata_casen$comuna=="Lo Espejo"])

Gini(subdata_casen$yautcor[subdata_casen$comuna=="Iquique"], corr = FALSE, na.rm = TRUE)
Gini(subdata_casen$yautcor[subdata_casen$comuna=="Vitacura"], corr = FALSE, na.rm = TRUE)
Gini(subdata_casen$yautcor[subdata_casen$comuna=="Santiago"], corr = FALSE, na.rm = TRUE)
Gini(subdata_casen$yautcor[subdata_casen$comuna=="Lo Barnechea"], corr = FALSE, na.rm = TRUE)
Gini(subdata_casen$yautcor[subdata_casen$comuna=="Aysén"], corr = FALSE, na.rm = TRUE)
Gini(subdata_casen$yautcor[subdata_casen$comuna=="Lo Espejo"], corr = FALSE, na.rm = TRUE)



## for loops

comunas <- unique(subdata_casen$comuna)
ncomunas <- length(comunas)

resultados <- matrix(NA, nrow = ncomunas, ncol = 3 ) 

row=1
for(i in comunas){
  resultados[row,1] <- mean(subdata_casen$yautcor[subdata_casen$comuna==i], na.rm = T)
  resultados[row,2] <- ratio9010(subdata_casen$yautcor[subdata_casen$comuna==i])
  resultados[row,3] <- Gini(subdata_casen$yautcor[subdata_casen$comuna==i], corr = FALSE, na.rm = TRUE)
  row = row + 1
}
  
colnames(resultados) <- c("promedio","r9010","gini")
rownames(resultados) <- comunas

resultados <- as.data.frame(resultados)

cor(resultados)

#Visualización


## Scatterplots
par(mfrow=c(2,2))

plot(resultados$promedio, resultados$gini, 
   xlab="Ingreso autonomo promedio en comuna", 
   ylab="Indice de Gini",
   type="p",  
   pch=16, 
   col="blue")

abline(lm(resultados$gini ~ resultados$promedio))


plot(resultados$gini, resultados$r9010, 
     xlab="Gini", 
     ylab="ratio 90/10", 
     type="p", 
     col="red")


plot(mi_reg$fitted.values, mi_reg$residuals, 
     xlab="y_hat", 
     ylab="residuos", 
     type="p", 
     pch=11, 
     col="green")

dev.off()


# otro
par(mfrow=c(1,1))
order <- order(resultados$gini)

plot(x=resultados$gini[order], y=seq(1:ncomunas),
     xlab="Indice de Gini",
     ylab="comuna", 
     type="b",  
     pch=43, 
     col="blue")

dev.off()


## Bar plots

barplot(prop.table(table(subdata_casen$educ)), col="purple", las=2)
box()
dev.off()
