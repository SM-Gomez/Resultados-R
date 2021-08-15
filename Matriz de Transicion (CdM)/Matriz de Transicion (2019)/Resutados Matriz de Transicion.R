library(ggplot2)
library(MASS)
library(mlogit)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)


#****************************************BASE DE DATOS 2019********************************************

df<-read_xlsx("MT2019.xlsx")


#****************************CREAMOS EL DATA FRAME QUE CONTIENE LAS CONVENCIONES***********************
Inicio_Fin<-c("Al Dia", "1-30 Dias " , "31-60 Dias ", " 61-90 Dias"," 91-120 Dias" ," 121-150 Dias",
              " 151-180 Dias"," 181-210 Dias"," 211-240 Dias"," 241-270 Dias",
              "271-300 Dias","301-330 Dias","331-360 Dias", "Mayor a 360 Dias")


Limite_Inferior <- c(0,1,31,61,91,121,151,181,211,241,271,301,331,361)


categoria<-c( "000-000","001-030","031-060","061-090","091-120","121-150","151-180","181-210",
              "211-240","241-270","271-300","301-330","331-360", "Mayor a 360")

df1<-data.frame(Inicio_Fin,Limite_Inferior,categoria)




#***************************ASIGNANDOLE CATEGORIA A LA VARIABLES RANGO INICIAL*************************

df<-cbind(df,"Rango Incial" = 0)

for (i in 1:length(df$`Dias de Mora Enero`)) {
  if(df$`Dias de Mora Enero`[i] == 0){
    df$`Rango Incial`[i] = "000-000"
    
  }else if (df$`Dias de Mora Enero`[i] >= 1 && df$`Dias de Mora Enero`[i] <= 30){
  df$`Rango Incial`[i] = "001-030"  
  
  }else if (df$`Dias de Mora Enero`[i] >= 31 && df$`Dias de Mora Enero`[i] <= 60){
    df$`Rango Incial`[i] = "031-060"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 61 && df$`Dias de Mora Enero`[i] <= 90){
    df$`Rango Incial`[i] = "061-090"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 91 && df$`Dias de Mora Enero`[i] <= 120){
    df$`Rango Incial`[i] = "091-120"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 121 && df$`Dias de Mora Enero`[i] <= 150){
    df$`Rango Incial`[i] = "121-150"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 151 && df$`Dias de Mora Enero`[i] <= 180){
    df$`Rango Incial`[i] = "151-180"
    
  }else if (df$`Dias de Mora Enero`[i] >= 181 && df$`Dias de Mora Enero`[i] <= 210){
    df$`Rango Incial`[i] = "181-210"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 211 && df$`Dias de Mora Enero`[i] <= 240){
    df$`Rango Incial`[i] = "211-240"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 241 && df$`Dias de Mora Enero`[i] <= 270){
    df$`Rango Incial`[i] = "241-270"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 271 && df$`Dias de Mora Enero`[i] <= 300){
    df$`Rango Incial`[i] = "271-300"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 301 && df$`Dias de Mora Enero`[i] <= 330){
    df$`Rango Incial`[i] = "301-330"  
    
  }else if (df$`Dias de Mora Enero`[i] >= 331 && df$`Dias de Mora Enero`[i] <= 360){
    df$`Rango Incial`[i] = "331-360"  
    
  }else{
    df$`Rango Incial`[i] = "Mayor a 360"
  }
}

#*****************************ASIGNANDOLE CATEGORIA A LA VARIABLES RANGO FINAL********************************
df<-cbind(df,"Rango Final" = 0)

for (i in 1:length(df$`Dias de Mora Diciembre`)) {
  if(df$`Dias de Mora Diciembre`[i] == 0){
    df$`Rango Final`[i] = "000-000"
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 1 && df$`Dias de Mora Diciembre`[i] <= 30){
    df$`Rango Final`[i] = "001-030"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 31 && df$`Dias de Mora Diciembre`[i] <= 60){
    df$`Rango Final`[i] = "031-060"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 61 && df$`Dias de Mora Diciembre`[i] <= 90){
    df$`Rango Final`[i] = "061-090"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 91 && df$`Dias de Mora Diciembre`[i] <= 120){
    df$`Rango Final`[i] = "091-120"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 121 && df$`Dias de Mora Diciembre`[i] <= 150){
    df$`Rango Final`[i] = "121-150"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 151 && df$`Dias de Mora Diciembre`[i] <= 180){
    df$`Rango Final`[i] = "151-180"
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 181 && df$`Dias de Mora Diciembre`[i] <= 210){
    df$`Rango Final`[i] = "181-210"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 211 && df$`Dias de Mora Diciembre`[i] <= 240){
    df$`Rango Final`[i] = "211-240"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 241 && df$`Dias de Mora Diciembre`[i] <= 270){
    df$`Rango Final`[i] = "241-270"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 271 && df$`Dias de Mora Diciembre`[i] <= 300){
    df$`Rango Final`[i] = "271-300"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 301 && df$`Dias de Mora Diciembre`[i] <= 330){
    df$`Rango Final`[i] = "301-330"  
    
  }else if (df$`Dias de Mora Diciembre`[i] >= 331 && df$`Dias de Mora Diciembre`[i] <= 360){
    df$`Rango Final`[i] = "331-360"  
    
  }else{
    df$`Rango Final`[i] = "Mayor a 360"
  }
}


#*****************************DATA FRAME PARA LA MATRIZ DE TRANSICION**************************

MT<-data.frame(df$`Rango Incial`,df$`Rango Final`)


#****************************CREANDO LA TABLA DINAMICA******************************
#install.packages("devtools")
library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
## Load rpivotTable
library(rpivotTable)

rpivotTable(df, rows="Rango Inicial", col="Rango Final", aggregatorName="Average", vals="value")



#***************************CALCULO DE LA MAXIMA MORA*************************************
#***********************EL VECTOR MORA MAXIAMA ALMACENA LA MAYOR MORA********************* 
#****************************COMPREDIDA DE FEBRERO-DICIEMBRE

df<-cbind(df,"Mora Maxima" = 0)
for (i in 1:length(df$`Dias de Mora Febrero`)) {
  df$`Mora Maxima`[i] = max(c(df$`Dias de Mora Febrero`[i],df$`Dias de Mora Marzo`[i],
                              df$`Dias de Mora Abril`[i],df$`Dias de Mora Mayo`[i],
                              df$`Dias de Mora Junio`[i],df$`Dias de Mora Julio`[i],
                              df$`Dias de Mora Agosto`[i],df$`Dias de Mora Septiembre`[i],
                              df$`Dias de Mora Octubre`[i],df$`Dias de Mora Noviembre`[i],
                              df$`Dias de Mora Diciembre`[i]))
}

#*****************************ASIGNANDOLE CATEGORIA A LA VARIABLES RANGO MAXIMO********************************
df<-cbind(df,"Rango Maximo" = 0)

for (i in 1:length(df$`Mora Maxima`)) {
  if(df$`Mora Maxima`[i] == 0){
    df$`Rango Maximo`[i] = "000-000"
    
  }else if (df$`Mora Maxima`[i] >= 1 && df$`Mora Maxima`[i] <= 30){
    df$`Rango Maximo`[i] = "001-030"  
    
  }else if (df$`Mora Maxima`[i] >= 31 && df$`Mora Maxima`[i] <= 60){
    df$`Rango Maximo`[i] = "031-060"  
    
  }else if (df$`Mora Maxima`[i] >= 61 && df$`Mora Maxima`[i] <= 90){
    df$`Rango Maximo`[i] = "061-090"  
    
  }else if (df$`Mora Maxima`[i] >= 91 && df$`Mora Maxima`[i] <= 120){
    df$`Rango Maximo`[i] = "091-120"  
    
  }else if (df$`Mora Maxima`[i] >= 121 && df$`Mora Maxima`[i] <= 150){
    df$`Rango Maximo`[i] = "121-150"  
    
  }else if (df$`Mora Maxima`[i] >= 151 && df$`Mora Maxima`[i] <= 180){
    df$`Rango Maximo`[i] = "151-180"
    
  }else if (df$`Mora Maxima`[i] >= 181 && df$`Mora Maxima`[i] <= 210){
    df$`Rango Maximo`[i] = "181-210"  
    
  }else if (df$`Mora Maxima`[i] >= 211 && df$`Mora Maxima`[i] <= 240){
    df$`Rango Maximo`[i] = "211-240"  
    
  }else if (df$`Mora Maxima`[i] >= 241 && df$`Mora Maxima`[i] <= 270){
    df$`Rango Maximo`[i] = "241-270"  
    
  }else if (df$`Mora Maxima`[i] >= 271 && df$`Mora Maxima`[i] <= 300){
    df$`Rango Maximo`[i] = "271-300"  
    
  }else if (df$`Mora Maxima`[i] >= 301 && df$`Mora Maxima`[i] <= 330){
    df$`Rango Maximo`[i] = "301-330"  
    
  }else if (df$`Mora Maxima`[i] >= 331 && df$`Mora Maxima`[i] <= 360){
    df$`Rango Maximo`[i] = "331-360"  
    
  }else{
    df$`Rango Maximo`[i] = "Mayor a 360"
  }
}


#***************************************CREANDO LA TABLA DINAMICA PARA COMPARAR***********************************
#***************************************RANGO INICIAL - RANGO MAXIMO**********************************************
rpivotTable(df, rows="Rango Inicial", col="Rango Maximo", aggregatorName="Average", vals="value")

