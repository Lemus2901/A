install.packages("olsrr")
install.packages("car")
install.packages("klaR")
library(olsrr)
library(car)
library(klaR)
install.packages("multiColl")
install.packages("leaps")
library(multiColl)
library(leaps)
source(file.choose()) #function.r
#-------escalado-------
modelo1 <- lm(mpg ~ disp + hp + cyl + wt, data = mtcars)
data <- data.frame(matrix(0, ncol = 1, nrow = 32))
data$mpg  <- mtcars$mpg
data$disp <- mtcars$disp
data$hp   <- mtcars$hp
data$cyl  <- mtcars$cyl
data$wt   <- mtcars$wt
str(data)
data2 <- scale(data[ , -1])
head(data2)
modelo2 <- lm(data2[ , 1] ~ -1 + data2[ , 2] + data2[ , 3] + data2[ , 4] + data2[ , 5])
summary(modelo2)#escalado
summary(modelo1)#no escalado
#multicolonealidad
modelo1 <- lm(mpg ~ disp + hp + cyl + wt, data = mtcars)
pairs(data2)
X <- model.matrix(modelo1)
cor(X[ , -1])
vif(modelo1) # sin problemas: menor 5; moderado (5,10] ; graves >10
cond.index(mpg ~ disp + hp + cyl + wt, data = mtcars) #cp sin problemas: menor 10; moderado (10, 31,62] ; graves >31,62
#seleccion modelos
modelo <- lm(mpg ~ disp + hp + cyl + wt, data = mtcars)
myAllRegTable(modelo)
#####--------------------------------------------
datos <- read.csv(file.choose())
str(datos)
#backward: empieza con todas y saca
#: x1 estudio, x2 excelenciia ,x3; sueno
modelo_Completo=lm(Calificacion ~  Estudio + Excelencia + Sueno, data = datos)
myAnova(modelo_Completo)
modelo_sin_X3=lm(Calificacion ~  Estudio + Excelencia, data = datos)
myAnova(modelo_sin_X3)
#fc=qf(#sigma,# R,Dferrorcompleto,lowetail=False)
fc=qf(0.05, df1 = 1, df2 = 96, lower.tail = FALSE)
fc
#sserm-ssefm/r  /msefm
Fp= ((14829.60 - 2299.75)/1)/23.9557
Fp>fc #rechaza h0 es significativa no se elimina
Fp
# forma corta #anova(modeloconmenospredictors,modeloconmaspredictoras)
anova(modelo_sin_X3, modelo_Completo) 
#nos fijamos en el p si es pequeno que alpha se rechaza h0
#forward
modelo_nulo=lm(Calificacion ~  1, data = datos)
modelo_x1=lm(Calificacion ~  Estudio, data = datos)
anova(modelo_nulo, modelo_x1)
modelo_x2=lm(Calificacion ~  Estudio+Excelencia, data = datos)
anova(modelo_x1, modelo_x2)
#codigo
# Suponiendo que tienes el modelo completo:
modelo_completo <- lm(Calificacion ~ Estudio + Excelencia + Sueno, data = datos)
#call es el modelo
# 1. Backward:
step(modelo_completo, direction = "backward")
# 2. Forward (necesitas definir un modelo nulo primero):
modelo_nulo <- lm(Calificacion ~ 1, data = datos)
step(modelo_nulo,
     scope = list(lower = modelo_nulo, upper = modelo_completo),
     direction = "forward")
#forward:
modelo_step <- ols_step_both_p(modelo_completo)
ols_step_forward_p(modelo_completo)

# 3. Stepwise (ambos):
step(modelo_completo, direction = "both")
#hipotesos
modelo_interaccion <- lm(price ~ bore * bodyStyle, data = datos)
summary(modelo_interaccion)
str(datos)
linearHypothesis(modelo_interaccion, "bodyStylesedan=0")
linearHypothesis(modelo_interaccion, "bore:bodyStylesedan=bore:bodyStylewagon")

df$genero <- as.factor(df$genero)
df$gen<-factor(df$genero, levels= c("Otro", "Masculino", "Femenino"))



