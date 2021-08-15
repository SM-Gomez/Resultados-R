library(ggplot2)
library(MASS)
library(mlogit)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)

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




#***************************************CREANDO EL MODELO DE REGRESION LOGISTICA********************************************
#MODELO DE RLB, PARA LA CREACION DE ESTE MODELO SE USO EL TEST DE WLAD 
modelo_de_prueba<-glm(`Estado de credito` ~ `Saldo de la cuota` + `Numero de cuotas` + `Monto del credito` +
          `Tasa interes` + `ingreso mensual` + Sexo + `Dependencia ocupacional`, family = "binomial",
        data = df2)
summary(modelo_de_prueba)


#**********************************************CÁLCULO DEL PARÁMETRO M******************************************************

#especificamos a R que estamos usando fechas 

df2$`Fecha de Analisis`<-as.Date(df2$`Fecha de Analisis`,"%Y-%m-%d")
df2$`Fecha de Vencimiento`<-as.Date(df2$`Fecha de Vencimiento`,"%Y-%m-%d")


#Añadiendo una columna nueva "Dias de Vencimiento" al data frame 
df2 <-cbind(df2, "Dias de Vencimiento"=as.Date(df2$`Fecha de Vencimiento`,"%Y-%m-%d") 
            - as.Date(df2$`Fecha de Analisis`,"%Y-%m-%d"))


#Añadiendo la columna "Años al Vencimiento" al data frame

df2<-cbind(df2,"Años al Vencimiento" = df2$`Dias de Vencimiento`/365)


#Añadiendo Parámetro M al data Frame

df2<-cbind(df2,"M" = 0)

#Llenando la columna del parámetro M
for (i in 1:length(df2$`Años al Vencimiento`)) {
  if(df2$`Años al Vencimiento`[i] < 0){
    df2$M[i] = 0
    
  }else if(df2$`Años al Vencimiento`[i] < 1){
    df2$M[i] = 1
    
  }else if(df2$`Años al Vencimiento`[i] < 3){
    df2$M[i] = 1.025
    
  }else if(df2$`Años al Vencimiento`[i] < 5){
    df2$M[i] = 1.05
    
  }else{
    df2$M[i] = 1.075
  }
}


#***********************************CÁLCULO DEL PARAMETRO LGD**********************************
#Asignamos el factor de recuperacion segun el tipo de garantia asociado 

for (j in 1:length(df2$`Tipo de Garantia`)){
  if(df2$`Tipo de Garantia`[j] == 0){
    df2$`Parámetro de la Garantia`[j] = 0/100
    
  }else if(df2$`Tipo de Garantia`[j] == 1){
    df2$`Parámetro de la Garantia`[j] = 100/100
    
  }else if(df2$`Tipo de Garantia`[j] == 2){
    df2$`Parámetro de la Garantia`[j] = 0/100
    
  }else if (df2$`Tipo de Garantia`[j] == 3){
    df2$`Parámetro de la Garantia`[j] = 85/100
  }
    
}


#Creamos la Variable Tasa de Recuperación del Crédito "f" donde: 
#f=(Valor_de_la_garantía*Parámetro_de_la_garantía)/Saldo_vigente_de_la_Operación


df2<-cbind(df2,"Tasa de Recuperacion" = 0)

for (k in 1:length(df2$`Valor de la Garantia`)){
  df2$`Tasa de Recuperacion`[k] = 
    (df2$`Valor de la Garantia`[k]*df2$`Parámetro de la Garantia`[k])/df2$`Saldo vigente`[k]
}

#Creamos la Variable LGD Original = 1-Tasa de Recuperacion
df2<-cbind(df2, "LGD Original" = 0)

for (l in 1: length(df2$`Tasa de Recuperacion`)) {
  df2$`LGD Original`[l] = (1 - df2$`Tasa de Recuperacion`[l])
  if (df2$`LGD Original`[l] < 0){
    df2$`LGD Original`[l] = percent(0)
    
  }else{
    df2$`LGD Original`[l] = percent((1 - df2$`Tasa de Recuperacion`[l])) 
  }
}


#**********************Valor numerico de LGD Original solo con fines de calculos***********************
df2<-cbind(df2, "LGD" = 0)

for (l in 1: length(df2$`Tasa de Recuperacion`)) {
  df2$`LGD`[l] = (1 - df2$`Tasa de Recuperacion`[l])
  if (df2$`LGD`[l] < 0){
    df2$`LGD`[l] = 0
    
  }else{
    df2$`LGD`[l] = round((1 - df2$`Tasa de Recuperacion`[l]),5) 
  }
}






#*******************Se Crea la Columna PD (Probabilidad de Incumplimiento)***************************
df2<-cbind(df2, PD = round (predict(modelo_de_prueba, type = "response"),5))

#******************Calculamos la PÉRDIDA ESPERADA por Cliente ***************************************
#**********************PE = EC.LGD.PD.M**************************************************************


df2<-cbind(df2, "Perdida Esperada" = 0 )
for (i in 1:length(df2$`Monto del credito`)){
  df2$`Perdida Esperada`[i] =  round( (df2$`Saldo vigente`[i]*df2$M[i]* df2$`LGD`[i] *df2$PD[i]),4)
  
}


#*****************************************CALCULO DE VaR*********************************************
#*****************************************PARAMETRO RHO**********************************************
df2<-cbind(df2, "Parametro RHO" = 0)
for (i in 1:length(df2$PD)) {
  df2$`Parametro RHO`[i] =round( (0.03*((1-exp(-35*df2$PD[i])))/(1-exp(-35)) 
                                 + 0.16*((1-(1-exp(-35*df2$PD[i]))))/(1-exp(-35))),6)
  
}


#**********************Valor Critico de la funcion de Distribucion************************************
#*********************************Normal Estandar*****************************************************
df2<-cbind(df2, "Inverso de la PD" = 0)
for (i in 1:length(df2$PD)) {
  df2$`Inverso de la PD`[i] = round( (qnorm(df2$PD[i],mean = 0, sd=1)),4)
}



#**********************************Inverso del 99.9%**************************************************
df2<-cbind(df2,"Inverso del 99.9%" = 0)
for (i in 1:length(df2$PD)) {
  df2$`Inverso del 99.9%`[i] =round ( (qnorm(0.999,mean=0,sd=1)),4)
}


#*********************************CALCULO DE Pmax*****************************************************
df2<-cbind(df2, "Probabilidad Maxima (Pmax)" = 0)
for (i in 1:length(df2$PD)) {
  df2$`Probabilidad Maxima (Pmax)`[i] = pnorm((1/(sqrt(1-df2$`Parametro RHO`[i]))*df2$`Inverso de la PD`[i] + 
                                                 sqrt(df2$`Parametro RHO`[i]/(1-df2$`Parametro RHO`[i]))*df2$`Inverso del 99.9%`[i]),
                                              mean = 0,sd=1)
}


#*********************************CALCULO FINAL DEL VaR***********************************************
df2<-cbind(df2,"Valor en Riesgo" = 0)
for (i in 1:length(df2$`Saldo vigente`)) {
  df2$`Valor en Riesgo`[i] =round( (df2$`Saldo vigente`[i]*df2$LGD[i]*df2$`Probabilidad Maxima (Pmax)`[i]),2)
}

#*************************************CALCULO DE LA PERDIDA INESPERADA********************************
#****************************************PI=VaR-PE****************************************************
df2<-cbind(df2,"Perdida Inesperada" = 0)
for (i in 1:length(df2$`Perdida Esperada`)) {
  df2$`Perdida Inesperada`[i] = round( (df2$`Valor en Riesgo`[i] - df2$`Perdida Esperada`[i]),2)
  
}


#************************************RESULTADOS CENTRALES*********************************************
PE<-sum(df2$`Perdida Esperada`)
VaR<-sum(df2$`Valor en Riesgo`)
PI<-sum(df2$`Perdida Inesperada`)
