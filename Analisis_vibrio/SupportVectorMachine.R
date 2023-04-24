#######################################################################
############### SVM 
pa=read.csv("VibrioTotal.csv")
library(pROC)

pa<-pa[,5:25]


library(caret)

#pa=na.omit(pa)

head(pa)

summary(pa)


head(pa)

############################



set.seed(5000) #pseudo-repeatability
trainIndex = createDataPartition(pa$Vibrio, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

training$Vibrio<-as.factor(training$Vibrio)
testing$Vibrio<-as.factor(testing$Vibrio)



## dos altas dos bajas
training = pa[c(1:12, 15:18,23),]


testing= pa[c(13,14,20:24),] #25% for model testing

training$Vibrio<-as.factor(training$Vibrio)
testing$Vibrio<-as.factor(testing$Vibrio)

training = pa[c(1:12, 15:18,23),]


testing= pa[c(13,14,20:24),] #25% for model testing




## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=500)


#methods: svmRadial rf  family=binomial(logit)


#svm with rbf kernel
rf_mod_fit=train(Vibrio~ .,
               data=training,trControl=train_control,method="rf")

svmRadialSigma_mod_fit=train(Vibrio~ Densidad,
               data=training,trControl=train_control,method="svmRadialSigma")


glm_Logit_mod_fit=train(Vibrio~  Densidad,
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

glm_Logit_mod_fit$levels


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
par(pty = "m")


modelosEval_Factor(training$Densidad,"Densidad",Exp_Densidad, training$Vibrio   , "Vibrio")
modelosEval_Factor(Salinidad$salnidad,"Salinidad",Exp_Salinidad, training$Vibrio, "Vibrio")

dev.off()

###############

Datos_Raster=read.csv("Datos_Raster.csv")
source("./Funciones/rasterizar_Variable.R")
library(raster)
library(rgdal)
library(oce)
library(dplyr)
costa<-rgdal::readOGR("./SIG_Datos/costa.shp")
rios<-readOGR("./SIG_Datos/rios_wgs84.shp")
estaciones<-readOGR("./SIG_Datos/estaciones.shp")
areas_protegidas<-readOGR("./SIG_Datos/areas_protegidas.shp")


#rasterizar_Variable(nombre_variable,longitud, latitud, variable, marea, leyenda)

Datos_Raster$MareaFactor
Datos_Raster_alta<-  subset(Datos_Raster,MareaFactor == "1")
Datos_Raster_baja<-  subset(Datos_Raster,MareaFactor == "0")
rasterizar_Variable("Densidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Densidad, "Alta", "Densidad del agua")
rasterizar_Variable("Densidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Densidad, "Baja", "Densidad del agua")


rasterVar<-list.files(path="./SIG_Datos/grids", pattern = ".tif$", full.names = TRUE)
rasterVar_names<-list.files(path="./SIG_Datos/grids", pattern = ".tif$", full.names = FALSE)

for(Archivos in 1:length(rasterVar_names)) assign(rasterVar_names[Archivos], raster(rasterVar[Archivos]))

plot(Densidad_Alta.tif)
plot(Densidad_Baja.tif)

Densidad_Alta.tif$layer
denalta<-as.data.frame(Densidad_Alta.tif$layer, xy=TRUE)


colnames(denalta)<-c("x","y","Densidad")

p1=predict(rf_mod_fit, newdata=denalta)


as.data.frame(p1)

denalta$Vibri_glm<-as.data.frame(p1)

denaltaPredi<-cbind(denalta$x,denalta$y,denalta$Vibri_glm)


colnames(denaltaPredi)<-c("x","y","Vibri_glm")


rasterPrediccion<-rasterFromXYZ(denaltaPredi)
plot(rasterPrediccion)
