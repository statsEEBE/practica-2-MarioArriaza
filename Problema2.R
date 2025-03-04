#Codigo para problema 2
mis_dades <- iris
x <- mis_dades$Petal.Length #estraigo en una lista las variables de petal.length
mean(x) #promedio de los datos petal.length
sd(x) #distancia tipica de los datos petal.length
hist(x)

y <- mis_dades$Sepal.Length
y
mean(y)
sd(y)

plot(x,y) #Al generar los graficos podemos ver como a medida que aumenta X tmb aumenta Y, hay una relación

#Calculo de la recta de regresión por mínimos cuadrados

#Calculo de m
m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m

#Calculo de b
b <- mean(y)-m*mean(x)
b

#Calculo de predicción en 1.5
m*1.5+b

#Solución modo rapido

mod <- lm(y~x)
mod
summary(mod)

data.frame(x=x)

ypred <- predict(mod, data.frame(x=x)) #Para la predicción en 1.5 uso data.frame(x=1.5)
plot(x, y)
lines(x, ypred)

Rsq <- sum((ypred-mean(y))^2)/sum((y-mean(y))^2) #Calculo de la variación de los datos respecto a la recta de regresión
Rsq
#Tengo una capacidad predictiva del 75 %
