#######################################################################
install.packages("pROC")

library(pROC)
library(caret)
library(raster)
library(rgdal)
library(oce)
library(dplyr)
library(ggpubr)
library(pROC) #glm
library(randomForest) #Random forest
library(e1071)

#Funciones
source("./Funciones/rasterizar_Variable.R")
source("./Funciones/modelosEval_Factor.R")

############### SVM 
VibrioData=read.csv("VibrioTotal.csv")
VibrioDataPCA<-VibrioData[,c(23:25)]
VibrioData<-VibrioData[,c(5:16,18:22)]

VibrioData<-VibrioData %>% 
    select(Vibrio, NO2, NO3, SST, Transparencia, Temperatura, Salinidad, PesoHum300 , q1)

#VibrioData=na.omit(VibrioData)

summary(VibrioData)
head(VibrioData)

############################

set.seed(750) #pseudo-repeatability
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
rf_mod_fit=train(Vibrio~ .,
               data=training,trControl=train_control,method="rf")

svmRadialSigma_mod_fit=train(Vibrio~ .,
               data=training,trControl=train_control,method="svmRadialSigma")


glm_Logit_mod_fit=train(Vibrio~ .,
               data=training,trControl=train_control,method="glm", family = "binomial")

set.seed (1)
knn_mod_fit=train(Vibrio~  ., 
                  data=training,trControl=train_control,method="knn")


RF_smm<-summary(rf_mod_fit)
svmRadialSigma_smm<-summary(svmRadialSigma_mod_fit)
glm_smm<-summary(glm_Logit_mod_fit)
knn_smm<-summary(knn_mod_fit)
capture.output("RF_smm",
               RF_smm,
               "svmRadialSigma_smm",
               svmRadialSigma_smm,
               "Rglm_smm",
               glm_smm,
               "knn_smm",
               knn_smm,
               file="Resultados_Modelos.txt"
               )

### for polynomial kernel specify method="svmPoly"

## importance of the different predictors
RF_varImp<-varImp(rf_mod_fit)
svm_varImp<-varImp(svmRadialSigma_mod_fit)
glm_varImp<-varImp(glm_Logit_mod_fit)
knn_varImp<-varImp(knn_mod_fit)




RF_varImpPlot<-ggplot(data= RF_varImp, aes(x=rownames(RF_varImp),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + ylab("Índice de Importancia")+  xlab("Variables")+
  ggtitle("Random Forest") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))


svm_varImpPlot<-ggplot(data= svm_varImp, aes(x=rownames(svm_varImp),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + ylab("Índice de Importancia")+xlab("Variables")+
  ggtitle("Support Vector Machine") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

glm_varImpPlot<-ggplot(data= glm_varImp, aes(x=rownames(glm_varImp),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + ylab("Índice de Importancia")+xlab("Variables")+
  ggtitle("Generalized Linear Models") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

knn_varImpPlot<-ggplot(data= knn_varImp, aes(x=rownames(knn_varImp),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + ylab("Índice de Importancia")+xlab("Variables")+
  ggtitle("k-Nearest Neighbors") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))



capture.output("RF_varImp",
               RF_varImp,
               "svm_varImp",
               svm_varImp,
               "glm_varImp",
               glm_varImp,
               "knn_varImp",
               knn_varImp,
               file="varImp_Modelos.txt"
)

png(filename = "./Imagenes/Importancia_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)
ggarrange( RF_varImpPlot, 
           svm_varImpPlot,
           glm_varImpPlot, 
           knn_varImpPlot, 
           ncol =  2, 
           nrow = 2)
dev.off()






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

png(filename = "./Imagenes/AUC_ML.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

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
    main= "AUC")

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
dev.off()

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
PC01<-VibrioDataPCA$PC01
PC02<-VibrioDataPCA$PC02            
PC03<-VibrioDataPCA$PC03
vibrioDatapcaTrain<-VibrioData$Vibrio

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
modelosEval_Factor(PC01,"PC01","PC01", vibrioDatapcaTrain, "Vibrio")
modelosEval_Factor(PC02,"PC02","PC02", vibrioDatapcaTrain, "Vibrio")            
modelosEval_Factor(PC03,"PC03","PC03", vibrioDatapcaTrain, "Vibrio") 
dev.off()

###############

Datos_Raster=read.csv("Datos_Raster.csv")

Datos_Raster$MareaFactor
Datos_Raster_alta<-  subset(Datos_Raster,MareaFactor == "1")
Datos_Raster_baja<-  subset(Datos_Raster,MareaFactor == "0")

rasterVar<-list.files(path="./SIG_Datos/grids", pattern = ".tif$", full.names = TRUE)
rasterVar_names<-list.files(path="./SIG_Datos/grids", pattern = ".tif$", full.names = FALSE)

for(Archivos in 1:length(rasterVar_names)) assign(rasterVar_names[Archivos], raster(rasterVar[Archivos]))

rasterizar_Variable("Salinidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Salinidad, "Alta", "Salinidad", Datos_Raster)
rasterizar_Variable("Salinidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Salinidad, "Baja","Salinidad", Datos_Raster)

rasterizar_Variable("Temperatura", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Temperatura, "Alta", "Temperatura", Datos_Raster)
rasterizar_Variable("Temperatura", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Temperatura, "Baja","Temperatura", Datos_Raster)

rasterizar_Variable("DensidadFito", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$DensidadFito , "Alta", "DensidadFito", Datos_Raster)
rasterizar_Variable("DensidadFito", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$DensidadFito , "Baja","DensidadFito", Datos_Raster)

rasterizar_Variable("Clorofila", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Clorofila, "Alta", "Clorofila   ", Datos_Raster)
rasterizar_Variable("Clorofila", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Clorofila, "Baja","Clorofila   ", Datos_Raster)



Temperatura_Alta<-as.data.frame(Temperatura_Alta.tif, xy=TRUE)
Temperatura_Baja<-as.data.frame(Temperatura_Baja.tif, xy=TRUE)

Salinidad_Alta<-as.data.frame(Salinidad_Alta.tif, xy=TRUE)
Salinidad_Baja<-as.data.frame(Salinidad_Baja.tif, xy=TRUE)

NO3_Alta<-as.data.frame(NO3_Alta.tif, xy=TRUE)
NO3_Baja<-as.data.frame(NO3_Baja.tif, xy=TRUE)

SST_Alta<-as.data.frame(SST_Alta.tif, xy=TRUE)
SST_Baja<-as.data.frame(SST_Baja.tif, xy=TRUE)



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




Temperatura_Alta<-as.data.frame(Temperatura_Alta.tif, xy=TRUE)
Temperatura_Baja<-as.data.frame(Temperatura_Baja.tif, xy=TRUE)

Salinidad_Alta<-as.data.frame(Salinidad_Alta.tif, xy=TRUE)
Salinidad_Baja<-as.data.frame(Salinidad_Baja.tif, xy=TRUE)

NO3_Alta<-as.data.frame(NO3_Alta.tif, xy=TRUE)
NO3_Baja<-as.data.frame(NO3_Baja.tif, xy=TRUE)

SST_Alta<-as.data.frame(SST_Alta.tif, xy=TRUE)
SST_Baja<-as.data.frame(SST_Baja.tif, xy=TRUE)

NO2_Alta<-as.data.frame(NO2_Alta.tif, xy=TRUE)
NO2_Baja<-as.data.frame(NO2_Baja.tif, xy=TRUE)

Transparencia_Alta<-as.data.frame(Transparencia_Alta.tif, xy=TRUE)
Transparencia_Baja<-as.data.frame(Transparencia_Baja.tif, xy=TRUE)

PesoHum300_Alta<-as.data.frame(PesoHum300_Alta.tif, xy=TRUE)
PesoHum300_Baja<-as.data.frame(PesoHum300_Baja.tif, xy=TRUE)

q1_Alta<-as.data.frame(q1_Alta.tif, xy=TRUE)
q1_Baja<-as.data.frame(q1_Baja.tif, xy=TRUE)






Alta_data_variables<-cbind(Temperatura_Alta$layer,
                           Salinidad_Alta$layer, 
                           NO3_Alta$layer, 
                           SST_Alta$layer,
                           NO2_Alta$layer,
                           Transparencia_Alta$layer,
                           PesoHum300_Alta$layer,
                           q1_Alta$layer
                           )



colnames(Alta_data_variables)<-c("Temperatura", 
                                 "Salinidad", 
                                 "NO3", 
                                 "SST",
                                 "NO2",
                                 "Transparencia",
                                 "PesoHum300",
                                 "q1"
                                 )

Baja_data_variables<-cbind(Temperatura_Baja$layer,
                           Salinidad_Baja$layer, 
                           NO3_Baja$layer, 
                           SST_Baja$layer,
                           NO2_Baja$layer,
                           Transparencia_Baja$layer,
                           PesoHum300_Baja$layer,
                           q1_Baja$layer)



colnames(Baja_data_variables)<-c("Temperatura", 
                                 "Salinidad", 
                                 "NO3", 
                                 "SST",
                                 "NO2",
                                 "Transparencia",
                                 "PesoHum300",
                                 "q1")

######Probabilidad
rf_Alta_predict=predict(rf_mod_fit, newdata=Alta_data_variables, type="prob") #Se puede predecir con distribución probabilistica predict (type="prob") o categórica (type="raw")
rf_Alta_predict_df<-as.data.frame(rf_Alta_predict)
rf_Alta<-cbind(Temperatura_Alta$x, Temperatura_Alta$y, rf_Alta_predict_df )
colnames(rf_Alta)<-c("Longitud","Latitud","Vibrio_rf_alta", "Probabilidad")
rf_Alta_rasterPrediccion<-rasterFromXYZ(rf_Alta)
plot(rf_Alta_rasterPrediccion)




rf_MA_prob<-ggplot(rf_Alta, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Probabilidad))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_gradientn(colours = c("#3288bd", 
                                   "#99d594", 
                                   "#e6f598", 
                                   "#ffffbf", 
                                   "#fee08b", 
                                   "#fc8d59", 
                                   "#d53e4f"))+
  labs(fill="Probabilidad", title= "RF - Marea Alta")
  

rf_Baja_predict=predict(rf_mod_fit, newdata=Baja_data_variables, type="prob")
rf_Baja_predict_df<-as.data.frame(rf_Baja_predict)
rf_Baja<-cbind(Temperatura_Baja$x, Temperatura_Baja$y, rf_Baja_predict_df )
colnames(rf_Baja)<-c("Longitud","Latitud","Vibrio_rf_Baja", "Probabilidad")
rasterPrediccion<-rasterFromXYZ(rf_Baja)
plot(rasterPrediccion)

rf_MB_prob<-ggplot(rf_Baja, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Probabilidad))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_gradientn(colours = c("#3288bd", 
                                   "#99d594", 
                                   "#e6f598", 
                                   "#ffffbf", 
                                   "#fee08b", 
                                   "#fc8d59", 
                                   "#d53e4f"))+
  labs(fill="Probabilidad", title= "RF - Marea Baja")


glm_Alta_predict=predict(glm_Logit_mod_fit, newdata=Alta_data_variables, type="prob")
glm_Alta_predict_df<-as.data.frame(glm_Alta_predict)
glm_Alta<-cbind(Temperatura_Alta$x, Temperatura_Alta$y, glm_Alta_predict_df )
colnames(glm_Alta)<-c("Longitud","Latitud","glm_Alta", "Probabilidad")
rasterPrediccion<-rasterFromXYZ(glm_Alta)
plot(rasterPrediccion)

glm_MA_prob<-ggplot(glm_Alta, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Probabilidad))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_gradientn(colours = c("#3288bd", 
                                   "#99d594", 
                                   "#e6f598", 
                                   "#ffffbf", 
                                   "#fee08b", 
                                   "#fc8d59", 
                                   "#d53e4f"))+
  labs(fill="Probabilidad", title= "glm - Marea Alta")

glm_Baja_predict=predict(glm_Logit_mod_fit, newdata=Baja_data_variables, type="prob")
glm_Baja_predict_df<-as.data.frame(glm_Baja_predict)
glm_Baja<-cbind(Temperatura_Baja$x, Temperatura_Baja$y, glm_Baja_predict_df )
colnames(glm_Baja)<-c("Longitud","Latitud","Vibrio_glm_Baja", "Probabilidad")
rasterPrediccion<-rasterFromXYZ(glm_Baja)
plot(rasterPrediccion)

glm_MB_prob<-ggplot(glm_Baja, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Probabilidad))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_gradientn(colours = c("#3288bd", 
                                   "#99d594", 
                                   "#e6f598", 
                                   "#ffffbf", 
                                   "#fee08b", 
                                   "#fc8d59", 
                                   "#d53e4f"))+
  labs(fill="Probabilidad", title= "glm - Marea Baja")

png(filename = "./Imagenes/Probabilidad_ML2.png", res = 300, width = 18, height = 24, units = "cm", pointsize = 13)

ggarrange( glm_MA_prob, glm_MB_prob,rf_MA_prob, rf_MB_prob, ncol =  2, nrow = 2,common.legend = F, legend ="bottom")
dev.off()




######Presencia####
rf_Alta_predict_raw=predict(rf_mod_fit, newdata=Alta_data_variables, type="raw") #Se puede predecir con distribución probabilistica predict (type="prob") o categórica (type="raw")
rf_Alta_predict_df_raw<-as.data.frame(rf_Alta_predict_raw)
rf_Alta_raw<-cbind(Temperatura_Alta$x, Temperatura_Alta$y, rf_Alta_predict_df_raw )
colnames(rf_Alta_raw)<-c("Longitud","Latitud", "Presencia")
rf_Alta_rasterPrediccion_raw<-rasterFromXYZ(rf_Alta_raw)
plot(rf_Alta_rasterPrediccion_raw)



rf_MA_raw<-ggplot(rf_Alta_raw, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Presencia))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values =  c("#d53e4f"))+
  labs(fill="Incidencia", title= "RF - Marea Alta")


rf_Baja_predict_raw=predict(rf_mod_fit, newdata=Baja_data_variables, type="raw")
rf_Baja_predict_df_raw<-as.data.frame(rf_Baja_predict_raw)
rf_Baja_raw<-cbind(Temperatura_Baja$x, Temperatura_Baja$y, rf_Baja_predict_df_raw )
colnames(rf_Baja_raw)<-c("Longitud","Latitud","Presencia")
rasterPrediccion_raw<-rasterFromXYZ(rf_Baja_raw)
plot(rasterPrediccion)

rf_MB_raw<-ggplot(rf_Baja_raw, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Presencia))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values = c("#3288bd", 
                                   "#d53e4f"))+
  labs(fill="Incidencia", title= "RF - Marea Baja")


glm_Alta_predict_raw=predict(glm_Logit_mod_fit, newdata=Alta_data_variables, type="raw")
glm_Alta_predict_df_raw<-as.data.frame(glm_Alta_predict_raw)
glm_Alta_raw<-cbind(Temperatura_Alta$x, Temperatura_Alta$y, glm_Alta_predict_df_raw )
colnames(glm_Alta_raw)<-c("Longitud","Latitud","Presencia")
rasterPrediccion<-rasterFromXYZ(glm_Alta_raw)
plot(rasterPrediccion_raw)

glm_MA_raw<-ggplot(glm_Alta_raw, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Presencia))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values = c("#d53e4f", 
                               "#d53e4f"))+
  labs(fill="Incidencia", title= "glm - Marea Alta")

glm_Baja_predict_raw=predict(glm_Logit_mod_fit, newdata=Baja_data_variables, type="raw")
glm_Baja_predict_df_raw<-as.data.frame(glm_Baja_predict_raw)
glm_Baja_raw<-cbind(Temperatura_Baja$x, Temperatura_Baja$y, glm_Baja_predict_df_raw )
colnames(glm_Baja_raw)<-c("Longitud","Latitud", "Presencia")
rasterPrediccion_raw<-rasterFromXYZ(glm_Baja_raw)
plot(rasterPrediccion_raw)

glm_MB_raw<-ggplot(glm_Baja_raw, aes(Longitud, Latitud)) +
  geom_tile(aes(fill = Presencia))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", linewidth=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_manual(values = c("#3288bd", 
                               "#d53e4f"))+
  labs(fill="Incidencia", title= "glm - Marea Baja")

png(filename = "./Imagenes/Incidencia_ML.png", res = 300, width = 18, height = 24, units = "cm", pointsize = 13)

ggarrange( glm_MA_raw, glm_MB_raw,rf_MA_raw, rf_MB_raw, ncol =  2, nrow = 2,common.legend = FALSE, legend ="bottom")
dev.off()
