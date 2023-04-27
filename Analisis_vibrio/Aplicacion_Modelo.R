#Título del script: Aplicación de
#Autores: Christian Bermúdez-Rivas 
#Objetivo: Realizar los análisis de los datos físico químicos.
#Lenguaje: R
#Fecha: Junio 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################

library(pROC) #glm
library(randomForest) #Random forest
library(e1071)#SVM


source("./Funciones/modelosEval_Factor.R")

vibrio=read.csv("VibrioTotal.csv")










colnames(vibrio)

vibrioData<-vibrio$Vibrio
vibrio$vibrio<-vibrio$Vibrio


Marea<-vibrio$MareaFactor
NO2<-vibrio$NO2
NO3<-vibrio$NO3
PO4<-vibrio$PO4
SiO2<-vibrio$SiO2
Clorofila<-vibrio$Clorofila
pH<-vibrio$pH
OD<-vibrio$OD 
Transparencia<-vibrio$Transparencia
SST<-vibrio$SST
Temperatura<-vibrio$Temperatura
Salinidad<-vibrio$Salinidad
Densidad<-vibrio$Densidad         
DensidadFito<-vibrio$DensidadFito
q0<-vibrio$q0
q1<-vibrio$q1
PesoHum500<-vibrio$PesoHum500
PesoHum300<-vibrio$PesoHum300
PC01<-vibrio$PC01
PC02<-vibrio$PC02            
PC03<-vibrio$PC03


Exp_NO2= expression(paste("Nitritos [NO"[2]^"-","] [",mu,"M]"))
Exp_NO3= expression(paste("Nitratos [NO"[3]^"-","] [",mu,"M]"))
Exp_PO4= expression(paste("Fosfatos [PO"[4]^"-3","] [",mu,"M]"))
Exp_SiO2= expression(paste("Silicatos [SiO"[2],"] [",mu,"M]"))
Exp_Clorofila=expression(paste("Clorofila ",alpha, " [",mu,"g.L"^-1,"]"))
Exp_pH="pH"
Exp_OD=expression(paste("Oxígeno Dis. [mg O"[2],".L"^-1,"]"))
Exp_Transparencia="Transparencia (m)"
Exp_SST=expression(paste("Sol.Susp.Tot.[mg.L"^-1,"]"))
Exp_Temperatura=expression(paste("Temperatura (°C)"))
Exp_Salinidad=" Salinidad (PSU)"
Exp_Densidad=expression(paste("Densidad del agua (kg.m"^-3,"]"))
Exp_DensidadFito=expression(paste("Densidad total del fitoplacton (Cel.L"^-1,"]"))
Exp_PesoHum500=expression(paste("Peso Hum. Zoo. 500 µm (g.m"^-3,"]"))
Exp_PesoHum300=expression(paste("Peso Hum. Zoo. 300 µm (g.m"^-3,"]"))
Exp_q0= expression(paste(""^0,"D"))
Exp_q1= expression(paste(""^1,"D"))
Exp_PC01="PC01"
Exp_PC02="PC02"
Exp_PC03="PC03"

#modelosEval_Factor(variable, nombre_variable, ejex, Factor, nombre_factor)
modelosEval_Factor(NO2,"NO2",Exp_NO2, vibrioData, "Vibrio")
modelosEval_Factor(NO3,"NO3",Exp_NO3, vibrioData, "Vibrio")
modelosEval_Factor(PO4,"PO4",Exp_PO4, vibrioData, "Vibrio")
modelosEval_Factor(SiO2,"SiO2",Exp_SiO2, vibrioData, "Vibrio")
modelosEval_Factor(Clorofila,"Clorofila",Exp_Clorofila, vibrioData, "Vibrio")
modelosEval_Factor(pH,"pH",Exp_pH, vibrioData, "Vibrio")
modelosEval_Factor(OD,"OD",Exp_OD, vibrioData, "Vibrio")
modelosEval_Factor(SST,"SST",Exp_SST, vibrioData, "Vibrio")
modelosEval_Factor(Temperatura,"Temperatura_mean",Exp_Temperatura, vibrioData, "Vibrio")
modelosEval_Factor(Salinidad,"Salinidad",Exp_Salinidad, vibrioData, "Vibrio")
modelosEval_Factor(Densidad,"Densidad",Exp_Densidad, vibrioData, "Vibrio")
modelosEval_Factor(DensidadFito,"Densidad de Fitoplacton",Exp_DensidadFito, vibrioData, "Vibrio")
modelosEval_Factor(PesoHum500,"Peso Hum.Zoo.500 µm",Exp_PesoHum500, vibrioData, "Vibrio")
modelosEval_Factor(PesoHum300,"Peso Hum.Zoo.500 µm",Exp_PesoHum300, vibrioData, "Vibrio")
modelosEval_Factor(q0,"Riqueza",Exp_q0, vibrioData, "Vibrio")
modelosEval_Factor(q1,"Diversidad",Exp_q1, vibrioData, "Vibrio")
modelosEval_Factor(PC01,"PC01","PC01", vibrioData, "Vibrio")
modelosEval_Factor(PC02,"PC02","PC02", vibrioData, "Vibrio")            
modelosEval_Factor(PC03,"PC03","PC03", vibrioData, "Vibrio") 
dev.off()


