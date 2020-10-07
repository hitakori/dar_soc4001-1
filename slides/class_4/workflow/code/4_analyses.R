#################### Basic data analysis  #################### 

# descriptives

mean(essdata_sub$age, na.rm = T)
sd(essdata_sub$age, na.rm = T)
table(essdata_sub$gndr_string)

sapply(essdata_sub[,c("eisced", "age")], mean, na.rm=TRUE) 
sapply(essdata_sub[,c("eisced", "age")], sd, na.rm=TRUE) 


# by group

library(psych)
summary_table <- describeBy(essdata_sub[,c("eisced", "age")], essdata_sub$cntry)


#################### Plots  #################### 

library(scales)

dev.off()

filename <- paste0(dirresults,"miprimerplot.jpeg")
jpeg(file=filename, width=600, height=350)

par(mfrow=c(1,2))


# plot izquierda

unique(essdata$eisced)
essdata$eisced <- ifelse(essdata$eisced==55, NA, essdata$eisced)

#plot(essdata$eisced, essdata$hinctnta)
plot(jitter(essdata$eisced,1), jitter(essdata$hinctnta,1), pch=16, col=alpha("blue", 0.01))

countries <- unique(essdata$cntry)

for (c in countries) {
  data_country <- essdata[essdata$cntry==c,] 
  abline(lm(hinctnta ~ eisced, data=data_country))
}



# plot derecha
hist(essdata$eisced)

dev.off()



################ Modelos de Regresión ################## 


# Fit linear model

model1 <- lm(eisced ~ age + factor(gndr_string), data=essdata_sub); model1


# Update modelo existente 
model2 <- update(model1, . ~ . + factor(gndr_string)*age); model2


# acceder aloutput 

names(model2)

model2$coefficients 
model2$residuals 


# summary del modelo

sum_model2 <- summary(model2); sum_model2 
names(sum_model2)

sum_model2$r.squared
sum_model2$sigma


# exportar resultados a una linda tabla 

library("stargazer")

filename <- paste0(dirresults,"miprimeratabladeregresion.txt")

stargazer(model1, model2, type="text",
dep.var.labels=c("Education","Education"),
covariate.labels=c("Age","Age2","Gender (Male=1)", "Gender*Age","Gender*Age2","Intercep"),
 out=filename)
