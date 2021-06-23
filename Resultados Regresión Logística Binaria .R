library(ggplot2)
library(MASS)
library(mlogit)
library(dplyr)
library(tidyr)
library(readxl)

#PARA CORRER EL CODIGO DAR CLIC EN RUM O OPRIMIR AL MISMO TIEMPO LAS TECLAS CONTROL+INTRO
#EN CADA LINEA DE CODIGO


#LEYENDO LA BASE DATOS
df2<-read_xlsx("baseRLB.xlsx")


#VERIFICAMOS SI HAY DATOS FALTANTES EN NUESTRO DATAFRAME
any(is.na(df2))


#VERIFICAMOS SI LOS DATOS SON DE TIPO NUMERICO
#Es importante estudiar el tipo de variable que estamos ocupando, 
#Saber si es texto, fecha, entero, decimal, etc. 
#Y en caso que detecte alguna variable del tipo "Factor",
#es importante cambiarla a una variable del tipo numerico.
str(df2)

#MODELO DE RLB, PARA LA CREACION DE ESTE MODELO SE USO EL TEST DE WLAD 
modelo_de_prueba<-glm(`Estado de credito` ~ `Saldo de la cuota` + `Numero de cuotas` + `Monto del credito` +
          `Tasa interes` + `ingreso mensual` + Sexo + `Dependencia ocupacional`, family = "binomial",
        data = df2)
summary(modelo_de_prueba)







