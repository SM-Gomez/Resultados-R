library(ggplot2)
library(MASS)
library(mlogit)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)

#**************************************BASE DE DATOS 2020*********************************************************
df2<-read_xlsx("MT2020.xlsx")

#***************************ASIGNANDOLE CATEGORIA A LA VARIABLES RANGO INICIAL*************************

df2<-cbind(df2,"Rango Incial" = 0)

for (i in 1:length(df2$`Dias de Mora Febrero`)) {
  if(df2$`Dias de Mora Febrero`[i] == 0){
    df2$`Rango Incial`[i] = "000-000"
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 1 && df2$`Dias de Mora Febrero`[i] <= 30){
    df2$`Rango Incial`[i] = "001-030"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 31 && df2$`Dias de Mora Febrero`[i] <= 60){
    df2$`Rango Incial`[i] = "031-060"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 61 && df2$`Dias de Mora Febrero`[i] <= 90){
    df2$`Rango Incial`[i] = "061-090"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 91 && df2$`Dias de Mora Febrero`[i] <= 120){
    df2$`Rango Incial`[i] = "091-120"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 121 && df2$`Dias de Mora Febrero`[i] <= 150){
    df2$`Rango Incial`[i] = "121-150"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 151 && df2$`Dias de Mora Febrero`[i] <= 180){
    df2$`Rango Incial`[i] = "151-180"
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 181 && df2$`Dias de Mora Febrero`[i] <= 210){
    df2$`Rango Incial`[i] = "181-210"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 211 && df2$`Dias de Mora Febrero`[i] <= 240){
    df2$`Rango Incial`[i] = "211-240"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 241 && df2$`Dias de Mora Febrero`[i] <= 270){
    df2$`Rango Incial`[i] = "241-270"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 271 && df2$`Dias de Mora Febrero`[i] <= 300){
    df2$`Rango Incial`[i] = "271-300"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 301 && df2$`Dias de Mora Febrero`[i] <= 330){
    df2$`Rango Incial`[i] = "301-330"  
    
  }else if (df2$`Dias de Mora Febrero`[i] >= 331 && df2$`Dias de Mora Febrero`[i] <= 360){
    df2$`Rango Incial`[i] = "331-360"  
    
  }else{
    df2$`Rango Incial`[i] = "Mayor a 360"
  }
}

#*****************************ASIGNANDOLE CATEGORIA A LA VARIABLES RANGO FINAL********************************
df2<-cbind(df2,"Rango Final" = 0)

for (i in 1:length(df2$`Dias de Mora Febrero 2021`)) {
  if(df2$`Dias de Mora Febrero 2021`[i] == 0){
    df2$`Rango Final`[i] = "000-000"
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 1 && df2$`Dias de Mora Febrero 2021`[i] <= 30){
    df2$`Rango Final`[i] = "001-030"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 31 && df2$`Dias de Mora Febrero 2021`[i] <= 60){
    df2$`Rango Final`[i] = "031-060"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 61 && df2$`Dias de Mora Febrero 2021`[i] <= 90){
    df2$`Rango Final`[i] = "061-090"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 91 && df2$`Dias de Mora Febrero 2021`[i] <= 120){
    df2$`Rango Final`[i] = "091-120"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 121 && df2$`Dias de Mora Febrero 2021`[i] <= 150){
    df2$`Rango Final`[i] = "121-150"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 151 && df2$`Dias de Mora Febrero 2021`[i] <= 180){
    df2$`Rango Final`[i] = "151-180"
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 181 && df2$`Dias de Mora Febrero 2021`[i] <= 210){
    df2$`Rango Final`[i] = "181-210"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i]>= 211 && df2$`Dias de Mora Febrero 2021`[i] <= 240){
    df2$`Rango Final`[i] = "211-240"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 241 && df2$`Dias de Mora Febrero 2021`[i] <= 270){
    df2$`Rango Final`[i] = "241-270"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 271 && df2$`Dias de Mora Febrero 2021`[i] <= 300){
    df2$`Rango Final`[i] = "271-300"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 301 && df2$`Dias de Mora Febrero 2021`[i] <= 330){
    df2$`Rango Final`[i] = "301-330"  
    
  }else if (df2$`Dias de Mora Febrero 2021`[i] >= 331 && df2$`Dias de Mora Febrero 2021`[i] <= 360){
    df2$`Rango Final`[i] = "331-360"  
    
  }else{
    df2$`Rango Final`[i] = "Mayor a 360"
  }
}


#****************************CREANDO LA TABLA DINAMICA******************************
#install.packages("devtools")
library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
## Load rpivotTable
library(rpivotTable)

rpivotTable(df2, rows="Rango Inicial", col="Rango Final", aggregatorName="Average", vals="value")



#***************************CALCULO DE LA MAXIMA MORA*************************************
#***********************EL VECTOR MORA MAXIAMA ALMACENA LA MAYOR MORA********************* 
#****************************COMPREDIDA DE FEBRERO 2020 - FEBRERO 2021********************

df2<-cbind(df2,"Mora Maxima" = 0)
for (i in 1:length(df2$`Dias de Mora Febrero`)) {
  df2$`Mora Maxima`[i] = max(c(df2$`Dias de Mora Marzo`[i],df2$`Dias de Mora Abril`[i],
                               df2$`Dias de Mora Mayo`[i],df2$`Dias de Mora Junio`[i],
                               df2$`Dias de Mora Julio`[i],df2$`Dias de Mora Agosto`[i],
                               df2$`Dias de Mora Septiembre`[i],df2$`Dias de Mora Octubre`[i],
                               df2$`Dias de Mora Noviembre`[i],df2$`Dias de Mora Diciembre`[i],
                               df2$`Dias de Mora Enero`[i],df2$`Dias de Mora Febrero 2021`[i]))
}

#*****************************ASIGNANDOLE CATEGORIA A LA VARIABLES RANGO MAXIMO********************************
df2<-cbind(df2,"Rango Maximo" = 0)

for (i in 1:length(df2$`Mora Maxima`)) {
  if(df2$`Mora Maxima`[i] == 0){
    df2$`Rango Maximo`[i] = "000-000"
    
  }else if (df2$`Mora Maxima`[i] >= 1 && df2$`Mora Maxima`[i] <= 30){
    df2$`Rango Maximo`[i] = "001-030"  
    
  }else if (df2$`Mora Maxima`[i] >= 31 && df2$`Mora Maxima`[i] <= 60){
    df2$`Rango Maximo`[i] = "031-060"  
    
  }else if (df2$`Mora Maxima`[i] >= 61 && df2$`Mora Maxima`[i] <= 90){
    df2$`Rango Maximo`[i] = "061-090"  
    
  }else if (df2$`Mora Maxima`[i] >= 91 && df2$`Mora Maxima`[i] <= 120){
    df2$`Rango Maximo`[i] = "091-120"  
    
  }else if (df2$`Mora Maxima`[i] >= 121 && df2$`Mora Maxima`[i] <= 150){
    df2$`Rango Maximo`[i] = "121-150"  
    
  }else if (df2$`Mora Maxima`[i] >= 151 && df2$`Mora Maxima`[i] <= 180){
    df2$`Rango Maximo`[i] = "151-180"
    
  }else if (df2$`Mora Maxima`[i] >= 181 && df2$`Mora Maxima`[i] <= 210){
    df2$`Rango Maximo`[i] = "181-210"  
    
  }else if (df2$`Mora Maxima`[i] >= 211 && df2$`Mora Maxima`[i] <= 240){
    df2$`Rango Maximo`[i] = "211-240"  
    
  }else if (df2$`Mora Maxima`[i] >= 241 && df2$`Mora Maxima`[i] <= 270){
    df2$`Rango Maximo`[i] = "241-270"  
    
  }else if (df2$`Mora Maxima`[i] >= 271 && df2$`Mora Maxima`[i] <= 300){
    df2$`Rango Maximo`[i] = "271-300"  
    
  }else if (df2$`Mora Maxima`[i] >= 301 && df2$`Mora Maxima`[i] <= 330){
    df2$`Rango Maximo`[i] = "301-330"  
    
  }else if (df2$`Mora Maxima`[i] >= 331 && df2$`Mora Maxima`[i] <= 360){
    df2$`Rango Maximo`[i] = "331-360"  
    
  }else{
    df2$`Rango Maximo`[i] = "Mayor a 360"
  }
}


#***************************************CREANDO LA TABLA DINAMICA PARA COMPARAR***********************************
#***************************************RANGO INICIAL - RANGO MAXIMO**********************************************
rpivotTable(df2, rows="Rango Inicial", col="Rango Maximo", aggregatorName="Average", vals="value")





