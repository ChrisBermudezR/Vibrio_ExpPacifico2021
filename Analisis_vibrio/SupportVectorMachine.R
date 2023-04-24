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


VibrioData<-VibrioData[,5:25]




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



## dos altas dos bajas
training = VibrioData[c(1:12, 15:18,23),]
testing= VibrioData[c(13,14,20:24),] #25% for model testing
training$Vibrio<-as.factor(training$Vibrio)
testing$Vibrio<-as.factor(testing$Vibrio)
training = VibrioData[c(1:12, 15:18,23),]
testing= VibrioData[c(13,14,20:24),] #25% for model testing




## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=500)


#methods: svmRadial rf  family=binomial(logit)


#svm with rbf kernel
rf_mod_fit=train(Vibrio~ Densidad + Salinidad,
               data=training,trControl=train_control,method="rf")

svmRadialSigma_mod_fit=train(Vibrio~ Densidad + Salinidad,
               data=training,trControl=train_control,method="svmRadialSigma")


glm_Logit_mod_fit=train(Vibrio~  Densidad + Salinidad,
               data=training,trControl=train_control,method="glm", family = "binomial")



summary(rf_mod_fit)
summary(svmRadialSigma_mod_fit)
summary(glm_Logit_mod_fit)

### for polynomial kernel specify method="svmPoly"

## importance of the different predictors
varImp(rf_mod_fit)
varImp(svmRadialSigma_mod_fit)
varImp(glm_Logit_mod_fit)

## test the model
rf_mod_fit_predict=as.numeric(predict(rf_mod_fit, newdata=testing)) #predict on the test data
svmRadialSigma_mod_fit_predict=as.numeric(predict(svmRadialSigma_mod_fit, newdata=testing))
glm_Logit_mod_fit_predict=as.numeric(predict(glm_Logit_mod_fit, testing))
#test model fit-auc

rf_matriz_confusion <- confusionMatrix(predict(rf_mod_fit, newdata=testing), testing$Vibrio)
svmRadialSigma_matriz_confusion <- confusionMatrix(predict(svmRadialSigma_mod_fit, newdata=testing), testing$Vibrio)
glm_matriz_confusion <- confusionMatrix(predict(glm_Logit_mod_fit, newdata=testing), testing$Vibrio)


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

legend("bottomright", legend=c("Regr. Logistica", "Random Forest", "SVM"), col=c("#a6cee3", "#1f78b4", "#b2df8a"), lwd=4)
VibrioDatar(pty = "m")


modelosEval_Factor(training$Densidad,"Densidad",Exp_Densidad, training$Vibrio   , "Vibrio")
modelosEval_Factor(Salinidad$salnidad,"Salinidad",Exp_Salinidad, training$Vibrio, "Vibrio")

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
rasterizar_Variable("Salinidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Salinidad, "Alta", "Densidad del agua", Datos_Raster)
rasterizar_Variable("Densidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Densidad, "Baja", "Densidad del agua")





rasterVar<-list.files(VibrioDatath="./SIG_Datos/grids", VibrioDatattern = ".tif$", full.names = TRUE)
rasterVar_names<-list.files(VibrioDatath="./SIG_Datos/grids", VibrioDatattern = ".tif$", full.names = FALSE)

for(Archivos in 1:length(rasterVar_names)) assign(rasterVar_names[Archivos], raster(rasterVar[Archivos]))

plot(Densidad_Alta.tif)
plot(Densidad_Baja.tif)

Densidad_Alta.tif$layer
denalta<-as.data.frame(Densidad_Alta.tif$layer, xy=TRUE)

colnames(pts.grid)<-c("x","y","Salinidad")


cbind(densidadModelo, pts.grid$Salinidad)
densidadModelo<-cbind(densidadModelo, pts.grid$Salinidad)
colnames(densidadModelo)<-c("x","y","Densidad","Salinidad")
p1=predict(glm_Logit_mod_fit, newdata=densidadModelo)




as.data.frame(p1)
densidadModelo$Vibri_glm<-as.data.frame(p1)
denaltaPredi<-cbind(densidadModelo$x,densidadModelo$y,densidadModelo$Vibri_glm)
colnames(denaltaPredi)<-c("Longitud","Latitud","Vibri_glm")
rasterPrediccion<-rasterFromXYZ(denaltaPredi)
plot(rasterPrediccion)



ggplot(denaltaPredi, aes(Longitud, Latitud)) +
  geom_raster(aes(fill = Vibri_glm))+
  geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#38a800", fill="#38a800") +
  #geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
  coord_sf(xlim = c(-78.595484615, -78.053463218), ylim = c(2.339426503 , 3.162306176), expand = FALSE)+   
  geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent", size=1) +
  theme(panel.background = element_rect(fill = '#bfe8ff', color="#737373"),
        panel.grid.major = element_line(color = '#252525', linetype = 'dotted'))+
  scale_fill_gradientn(colours = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"))+
  #geom_point(data=estaciones, aes(x= Longitud, y= Latitud_De))+
  labs(fill="Probabilidad Vibrio", title= "marea")
  
