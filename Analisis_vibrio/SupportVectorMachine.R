#######################################################################
library(pROC)
library(caret)

source("./Funciones/rasterizar_Variable.R")
library(raster)
library(rgdal)
library(oce)
library(dplyr)
############### SVM 
VibrioData=read.csv("VibrioTotal.csv")


VibrioData<-VibrioData[,c(5:16,18:25)]




#VibrioData=na.omit(VibrioData)

summary(VibrioData)
head(VibrioData)

############################



set.seed(10000) #pseudo-repeatability
trainIndex = caret::createDataPartition(VibrioData$Vibrio, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = VibrioData[ trainIndex,] #75% data for model training
testing= VibrioData[-trainIndex,] #25% for model testing

training$Vibrio<-as.factor(training$Vibrio)
testing$Vibrio<-as.factor(testing$Vibrio)






## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", 
                             number=10)


#methods: svmRadial rf  family=binomial(logit)


#svm with rbf kernel
rf_mod_fit=train(Vibrio~ Salinidad + Temperatura + DensidadFito + Clorofila,
               data=training,trControl=train_control,method="rf")

svmRadialSigma_mod_fit=train(Vibrio~ Salinidad + Temperatura + DensidadFito + Clorofila,
               data=training,trControl=train_control,method="svmRadialSigma")


glm_Logit_mod_fit=train(Vibrio~ Salinidad + Temperatura + DensidadFito + Clorofila,
               data=training,trControl=train_control,method="glm", family = "binomial")

knn_mod_fit=train(Vibrio~  Salinidad + Temperatura + DensidadFito + Clorofila, 
                  data=training,trControl=train_control,method="knn")


summary(rf_mod_fit)
summary(svmRadialSigma_mod_fit)
summary(glm_Logit_mod_fit)
summary(knn_mod_fit)
### for polynomial kernel specify method="svmPoly"

## importance of the different predictors
varImp(rf_mod_fit)
varImp(svmRadialSigma_mod_fit)
varImp(glm_Logit_mod_fit)
varImp(knn_mod_fit)

## test the model
rf_mod_fit_predict=as.numeric(predict(rf_mod_fit, newdata=testing))
svmRadialSigma_mod_fit_predict=as.numeric(predict(svmRadialSigma_mod_fit, newdata=testing))
glm_Logit_mod_fit_predict=as.numeric(predict(glm_Logit_mod_fit, newdata=testing))
knn_mod_fit_predict=as.numeric(predict(knn_mod_fit, newdata=testing))
#test model fit-auc


confusionMatrix(predict(rf_mod_fit, newdata=testing), testing$Vibrio)$overall[1]
confusionMatrix(predict(svmRadialSigma_mod_fit, newdata=testing), testing$Vibrio)$overall[1]
confusionMatrix(predict(glm_Logit_mod_fit, newdata=testing), testing$Vibrio)$overall[1]
confusionMatrix(predict(knn_mod_fit, newdata=testing), testing$Vibrio)$overall[1]


roc(testing[,"Vibrio"], 
    glm_Logit_mod_fit_predict, 
    plot=TRUE, 
    legacy.axes=TRUE, 
    percent=TRUE,
    xlab="Porcentage de Falsos Positivos",
    ylab="Porcentage de Falsos Negativos",
    col="#a6cee3",
    lwd=4,
    print.auc=TRUE,
    print.auc.x=45,
    main= "Gráfica")

plot.roc(testing[,"Vibrio"], 
         rf_mod_fit_predict,
         percent=TRUE, 
         col="#1f78b4", 
         lwd=4, 
         print.auc=TRUE, 
         add=TRUE, 
         print.auc.y=44,
         print.auc.x=45,)

plot.roc(testing[,"Vibrio"], 
         svmRadialSigma_mod_fit_predict,
         percent=TRUE, 
         col="#b2df8a", 
         lwd=4, 
         print.auc=TRUE, 
         add=TRUE, 
         print.auc.y=38,
         print.auc.x=45,)

plot.roc(testing[,"Vibrio"], 
         knn_mod_fit_predict,
         percent=TRUE, 
         col="red", 
         lwd=4, 
         print.auc=TRUE, 
         add=TRUE, 
         print.auc.y=33,
         print.auc.x=45,)

legend("bottomright", legend=c("Regr. Logistica", "Random Forest", "SVM", "KNN"), col=c("#a6cee3", "#1f78b4", "#b2df8a", "red"), lwd=4)
par(pty = "m")


###############################


vibrioData<-training$Vibrio
NO2<-training$NO2
NO3<-training$NO3
PO4<-training$PO4
SiO2<-training$SiO2
Clorofila<-training$Clorofila
pH<-training$pH
OD<-training$OD 
Transparencia<-training$Transparencia
SST<-training$SST
Temperatura<-training$Temperatura
Salinidad<-training$Salinidad
Densidad<-training$Densidad         
DensidadFito<-training$DensidadFito
q0<-training$q0
q1<-training$q1
PesoHum500<-training$PesoHum500
PesoHum300<-training$PesoHum300
PC01<-training$PC01
PC02<-training$PC02            
PC03<-training$PC03


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






###############

Datos_Raster=read.csv("Datos_Raster.csv")

costa<-rgdal::readOGR("./SIG_Datos/costa.shp")
rios<-readOGR("./SIG_Datos/rios_wgs84.shp")
estaciones<-readOGR("./SIG_Datos/estaciones.shp")
areas_protegidas<-readOGR("./SIG_Datos/areas_protegidas.shp")

Datos_Raster$MareaFactor
Datos_Raster_alta<-  subset(Datos_Raster,MareaFactor == "1")
Datos_Raster_baja<-  subset(Datos_Raster,MareaFactor == "0")

source("./Funciones/rasterizar_Variable.R")
#rasterizar_Variable(nombre_variable,longitud, latitud, variable, marea, leyenda)
rasterizar_Variable("Salinidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Salinidad, "Alta", "Salinidad", Datos_Raster)
rasterizar_Variable("Salinidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Salinidad, "Baja","Salinidad", Datos_Raster)

rasterizar_Variable("Temperatura", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Temperatura, "Alta", "Temperatura", Datos_Raster)
rasterizar_Variable("Temperatura", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Temperatura, "Baja","Temperatura", Datos_Raster)

rasterizar_Variable("DensidadFito", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$DensidadFito , "Alta", "DensidadFito", Datos_Raster)
rasterizar_Variable("DensidadFito", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$DensidadFito , "Baja","DensidadFito", Datos_Raster)

rasterizar_Variable("Clorofila", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Clorofila, "Alta", "Clorofila   ", Datos_Raster)
rasterizar_Variable("Clorofila", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Clorofila, "Baja","Clorofila   ", Datos_Raster)



rasterVar<-list.files(path="./SIG_Datos/grids", pattern = ".tif$", full.names = TRUE)
rasterVar_names<-list.files(path="./SIG_Datos/grids", pattern = ".tif$", full.names = FALSE)

for(Archivos in 1:length(rasterVar_names)) assign(rasterVar_names[Archivos], raster(rasterVar[Archivos]))

plot(Temperatura_Alta.tif)
plot(Temperatura_Baja.tif)
plot(Salinidad_Alta.tif)
plot(Salinidad_Baja.tif)
plot(DensidadFito_Alta.tif)
plot(DensidadFito_Baja.tif)
plot(Clorofila_Alta.tif)
plot(Clorofila_Baja.tif)




Temperatura_Alta<-as.data.frame(Temperatura_Alta.tif$layer, xy=TRUE)
Temperatura_Baja<-as.data.frame(Temperatura_Baja.tif$layer, xy=TRUE)

Salinidad_Alta<-as.data.frame(Salinidad_Alta.tif$layer, xy=TRUE)
Salinidad_Baja<-as.data.frame(Salinidad_Baja.tif$layer, xy=TRUE)

DensidadFito_Alta<-as.data.frame(DensidadFito_Alta.tif$layer, xy=TRUE)
DensidadFito_Baja<-as.data.frame(DensidadFito_Baja.tif$layer, xy=TRUE)

Clorofila_Alta<-as.data.frame(Clorofila_Alta.tif$layer, xy=TRUE)
Clorofila_Baja<-as.data.frame(Clorofila_Baja.tif$layer, xy=TRUE)

Alta_data_variables<-cbind(Temperatura_Alta$layer,Salinidad_Alta$layer, DensidadFito_Alta$layer, Clorofila_Alta$layer)
colnames(Alta_data_variables)<-c("Temperatura", "Salinidad", "DensidadFito", "Clorofila")

Baja_data_variables<-cbind(Temperatura_Baja$layer,Salinidad_Baja$layer, DensidadFito_Baja$layer, Clorofila_Baja$layer)
colnames(Baja_data_variables)<-c("Temperatura", "Salinidad","DensidadFito", "Clorofila")


rf_Alta_predict=predict(rf_mod_fit, newdata=Alta_data_variables)
rf_Alta_predict_df<-as.data.frame(rf_Alta_predict)
rf_Alta<-cbind(Temperatura_Alta$x, Temperatura_Alta$y, rf_Alta_predict_df )
colnames(rf_Alta)<-c("Longitud","Latitud","Vibrio_rf_alta")
rf_Alta_rasterPrediccion<-rasterFromXYZ(rf_Alta)
plot(rf_Alta_rasterPrediccion)



ggplot(rf_Alta, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Vibrio_rf_alta))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values = c("#ef8a62"))+
  #geom_point(data=estaciones, aes(x= Longitud, y= Latitud_De))+
  labs(fill="Probabilidad Vibrio", title= "marea")
  

rf_Baja_predict=predict(rf_mod_fit, newdata=Baja_data_variables)
rf_Baja_predict_df<-as.data.frame(rf_Baja_predict)
rf_Baja<-cbind(Temperatura_Baja$x, Temperatura_Baja$y, rf_Baja_predict_df )
colnames(rf_Baja)<-c("Longitud","Latitud","Vibrio_rf_Baja")
rasterPrediccion<-rasterFromXYZ(rf_Baja)
plot(rasterPrediccion)

ggplot(rf_Baja, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Vibrio_rf_Baja))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values = c("#67a9cf","#ef8a62"))+
  #geom_point(data=estaciones, aes(x= Longitud, y= Latitud_De))+
  labs(fill="Probabilidad Vibrio", title= "marea")


SVM_Alta_predict=predict(glm_Logit_mod_fit, newdata=Alta_data_variables)
SVM_Alta_predict_df<-as.data.frame(SVM_Alta_predict)
SVM_Alta<-cbind(Temperatura_Alta$x, Temperatura_Alta$y, SVM_Alta_predict_df )
colnames(SVM_Alta)<-c("Longitud","Latitud","SVM_Alta")
rasterPrediccion<-rasterFromXYZ(SVM_Alta)
plot(rasterPrediccion)

ggplot(SVM_Alta, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = SVM_Alta))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values = c("#67a9cf","#ef8a62"))+
  #geom_point(data=estaciones, aes(x= Longitud, y= Latitud_De))+
  labs(fill="Probabilidad Vibrio", title= "marea")

SVM_Baja_predict=predict(glm_Logit_mod_fit, newdata=Baja_data_variables)
SVM_Baja_predict_df<-as.data.frame(SVM_Baja_predict)
SVM_Baja<-cbind(Temperatura_Baja$x, Temperatura_Baja$y, SVM_Baja_predict_df )
colnames(SVM_Baja)<-c("Longitud","Latitud","Vibrio_SVM_Baja")
rasterPrediccion<-rasterFromXYZ(SVM_Baja)
plot(rasterPrediccion)

ggplot(SVM_Baja, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Vibrio_SVM_Baja))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values = c("#67a9cf","#ef8a62"))+
  #geom_point(data=estaciones, aes(x= Longitud, y= Latitud_De))+
  labs(fill="Probabilidad Vibrio", title= "marea")
