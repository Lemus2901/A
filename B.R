datos=read.csv(file.choose())
str(datos)

#convertir a vin
datos$hospit=ifelse(datos$pac_hos_==1, 1, 0)

#.-------------------------------------------------
#si quiere cambiar el nivel referencia
table(datos_ajuste$esq_vac)
datos$VAC=factor(datos$esq_vac,levels = c("Yes","No","Unknown")

#--------------------------------------

#separar datos en 1:3500 y otra prueba de 3501: 4421
datos_ajuste=datos[1:3500, ]
datos_prueba=datos[3501:4221, ]

#ajustar modelo estimar la prob de hospitalizacion ene funcion de la 
#tallla y el esquema de vacuncaion
modelo=glm(hospit~ talla_act + esq_vac, data = datos_ajuste, family = binomial(link = "logit"))
summary(modelo)

#evualiar signficancia general
#H0: B1=B2=B3=0   H1: Alguno distito a 0  (son 3 porque una es categorica con 3 variables Y se agarran 2 com una de referecia)
#usamos XC=D0**2 -D**2= APARECE EN Eel sumarry
#   Null deviance: 2996.9  on 3499  degrees of freedom
#Residual deviance: 2725.9  on 3496  degrees of freedom
#cambiar nive de ref para la variable esquemaa de vacunacion
xC=2996.9-2725.9
#SE REChaza H0 si xc>xp__p,0,01
#P numero de variables predicotras
XP=qchisq(0.05,3, lower.tail = FALSE)
xC>XP
# SE RECHAZA h0

# Interprete coeficiente del modelo
#por ejemplo en talla seria : por un incremento unitario en la talla se tiene una reduccion de 0.005 el log odds de estas hospitalizado
# o tambien: cuando aumenta la talla se reduce 

# si lusamo exp para interpretar
1-exp(-0.050019) # el de la talla
#por un incremento en la talla tenemos una disnminucion(ya que b es neg) de 4.8% 
#en la chances de ser hopsitalizados
1-exp(1.072217) #esquema_unk categorica
#cuando pasamos de esquema no a unk hay un 
#aumento de 192% en la probabilidades de ser hospitalizado


#evalue la capacidad predictiva deL MODELO
table(datos$pac_hos_)/sum(table(datos$pac_hos_))

#1: hosipitalizad 2:sin hospitalizacion proporicion

#matriz confusion
predicciones=predict.glm(modelo, newdata = datos_prueba, type =c("response"))
table(datos_prueba$hospit, predicciones>0.5)

TCC= sum(diag(table(datos_prueba$hospit, predicciones>0.5)))/sum(table(datos_prueba$hospit, predicciones>0.5))
TCC
# curva roc
install.packages("PRROC")
library(PRROC)
PROROC_OBJ <- roc.curve(scores.class0 = predicciones,
                        weights.class0 = datos_prueba$hospit,curve = TRUE)
plot(PROROC_OBJ)
