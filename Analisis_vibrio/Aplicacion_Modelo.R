library(pROC) # install with install.packages("pROC")
library(randomForest) 

source("modelosEval_Factor_Factor.R")

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
Salinidad<-vibrio$Salinidad
OD<-vibrio$OD          
SST<-vibrio$SST
Temperatura<-vibrio$Temperatura
Densidad_mean<-vibrio$Densidad
DensidadFito<-vibrio$DensidadFito
pH<-vibrio$pH
PC01<-vibrio$PC01
PC02<-vibrio$PC02            
PC03<-vibrio$PC03


Exp_NO2= expression(paste("Nitritos [NO"[2]^"-","] [",mu,"M]"))
Exp_NO3= expression(paste("Nitratos [NO"[3]^"-","] [",mu,"M]"))
Exp_PO4= expression(paste("Fosfatos [PO"[4]^"-3","] [",mu,"M]"))
Exp_SiO2= expression(paste("Silicatos [SiO"[2],"] [",mu,"M]"))
Exp_Clorofila2=expression(paste("Clorofila ",alpha, " [",mu,"g.L"^-1,"]"))
Exp_Salinidad2=" Salinidad (PSU)"
Exp_pH="pH"
Exp_OD=expression(paste("Oxígeno Dis. [mg O"[2],".L"^-1,"]"))
Exp_SST=expression(paste("Sol.Susp.Tot.[mg.L"^-1,"]"))
Exp_Temperatura_mean=expression(paste("Media Vert. de Temperatura (°C)"))
Exp_Salinidad_mean=expression(paste("Media Vert. de Salinidad (PSU)"))
Exp_Oxigeno_mean=expression(paste("Media Vert. de Oxi. [mg O"[2],".L"^-1,"]"))
Exp_Densidad_mean=expression(paste("Media Vert. de Densidad(kg.m"^-3,"]"))

modelosEval_Factor(NO2,"NO2",Exp_NO2)
modelosEval_Factor(NO3,"NO3",Exp_NO3)
modelosEval_Factor(PO4,"PO4",Exp_PO4)
modelosEval_Factor(SiO2,"SiO2",Exp_SiO2)
modelosEval_Factor(Clorofila,"Clorofila",Exp_Clorofila2)
modelosEval_Factor(Salinidad,"Salinidad",Exp_Salinidad2)
modelosEval_Factor(pH,"pH",Exp_pH)
modelosEval_Factor(OD,"OD",Exp_OD)
modelosEval_Factor(SST,"SST",Exp_SST)
modelosEval_Factor(Temperatura_mean,"Temperatura_mean",Exp_Temperatura_mean)
modelosEval_Factor(Salinidad_mean,"Salinidad_mean",Exp_Salinidad_mean)
modelosEval_Factor(Oxigeno_mean,"Oxigeno_mean",Exp_Oxigeno_mean)
modelosEval_Factor(Densidad_mean,"Densidad_mean",Exp_Densidad_mean)
modelosEval_Factor(PC01,"PC01","PC01")
modelosEval_Factor(PC02,"PC02","PC02")            
modelosEval_Factor(PC03,"PC03","PC03") 
modelosEval_Factor(DensidadFito,"Densidad de Fitoplacton","Densidad de Fitoplacton") 
