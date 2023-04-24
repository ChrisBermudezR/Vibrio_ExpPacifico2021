

########################
library(e1071)
source("./Funciones/clasificador_SVM.R")
vibrio=read.csv("VibrioTotal.csv")
colnames(vibrio)

clasificador_SVM(vibrio$Temperatura, 
                 vibrio$SST, 
                 vibrio$Vibrio, 
                 "Temperatura", 
                 "Salinidad",
                 "radial")


Datos_Alterados<-cbind(vibrio$Temperatura, vibrio$Densidad, vibrio$Vibrio)
Datos_Alterados<-as.data.frame(Datos_Alterados)
colnames(Datos_Alterados)<-c("Temperatura", "Densidad", "vibrio")
datos_SinAus<-Datos_Alterados[-14,]


clasificador_SVM(datos_SinAus$Temperatura, datos_SinAus$Densidad, datos_SinAus$vibrio, "Temperatura", "Salinidad")
