library(pROC) # install with install.packages("pROC")
library(randomForest) 


vibrio=read.csv("VibrioTotal.csv")


colnames(vibrio)
vibrioData<-vibrio$Vibrio

NO2<-vibrio$NO2
NO3<-vibrio$NO3
PO4<-vibrio$PO4
SiO2<-vibrio$SiO2
Clorofila<-vibrio$Clorofila
Salinidad<-vibrio$Salinidad
OD<-vibrio$OD          
SST<-vibrio$SST
Temperatura_mean<-vibrio$Temperatura_mean
Salinidad_mean<-vibrio$Salinidad_mean
Oxigeno_mean<-vibrio$Oxigeno_mean
Densidad_mean<-vibrio$Densidad_mean
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

modelosEval(NO2,"NO2",Exp_NO2)
modelosEval(NO3,"NO3",Exp_NO3)
modelosEval(PO4,"PO4",Exp_PO4)
modelosEval(SiO2,"SiO2",Exp_SiO2)
modelosEval(Clorofila,"Clorofila",Exp_Clorofila2)
modelosEval(Salinidad,"Salinidad",Exp_Salinidad2)
modelosEval(pH,"pH",Exp_pH)
modelosEval(OD,"OD",Exp_OD)
modelosEval(SST,"SST",Exp_SST)
modelosEval(Temperatura_mean,"Temperatura_mean",Exp_Temperatura_mean)
modelosEval(Salinidad_mean,"Salinidad_mean",Exp_Salinidad_mean)
modelosEval(Oxigeno_mean,"Oxigeno_mean",Exp_Oxigeno_mean)
modelosEval(Densidad_mean,"Densidad_mean",Exp_Densidad_mean)
modelosEval(PC01,"PC01","PC01")
modelosEval(PC02,"PC02","PC02")            
modelosEval(PC03,"PC03","PC03") 




modelosEval<-function(variable, nombre_variable, ejex){
  glm.fit=glm(vibrioData  ~ variable, family=binomial)
  resultados<-summary(glm.fit)
  capture.output(nombre_variable, glm.fit,resultados, file = paste0("./Resultados/",nombre_variable,"_glm.txt"))
  
  
png(filename = paste0("./Imagenes/",nombre_variable,"_glm.png"), width = 15, height = 15, units = "cm", pointsize = 15, res = 300)
  plot(x=variable, 
     y=vibrioData,
     xlab=ejex,
     ylab="Vibrio spp")
  lines(variable, glm.fit$fitted.values)

dev.off()
  
rf.model <- randomForest(factor(vibrioData) ~ variable)
capture.output(nombre_variable, rf.model, file = paste0("./Resultados/",nombre_variable,"_rf.txt"))


png(filename = paste0("./Imagenes/",nombre_variable,"_ROC.png"), width = 15, height = 15, units = "cm", pointsize = 15, res = 300)
roc(vibrioData, 
    glm.fit$fitted.values, 
    plot=TRUE, 
    legacy.axes=TRUE, 
    percent=TRUE,
    xlab="Porcentage de Falsos Positivos",
    ylab="Porcentage de Falsos Negativos",
    col="#377eb8",
    lwd=4,
    print.auc=TRUE,
    print.auc.x=45,
    main= ejex)
    
 plot.roc(vibrioData, 
         rf.model$votes[,1],
         percent=TRUE, 
         col="#4daf4a", 
         lwd=4, 
         print.auc=TRUE, 
         add=TRUE, 
         print.auc.y=44,
         print.auc.x=45,)

legend("bottomright", legend=c("Regresión Logistica", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)
par(pty = "m")
dev.off()

}



############################

library(pROC)
library(ggplot2)

# Calcular los valores de la curva ROC para cada modelo
logitROC <- roc(vibrioData, glm.fit$fitted.values)
rfROC <- roc(vibrioData, rf.model$votes[,1])

# Crear un dataframe con los valores de la curva ROC y la etiqueta de cada modelo
roc.df <- data.frame(
  false_positive_rate = logitROC$specificities, 
  true_positive_rate = rev(logitROC$sensitivities), 
  model = "Regresión Logistica"
)
roc.df <- rbind(roc.df, data.frame(
  false_positive_rate = rfROC$specificities, 
  true_positive_rate = rev(rfROC$sensitivities), 
  model = "Random Forest"
))

# Crear una gráfica de dispersión con ggplot() y añadir las curvas ROC con geom_line()
ggplot(data = roc.df, aes(x = false_positive_rate, y = true_positive_rate, color = model)) +
  geom_line(size = 1.5) +
  
  # Añadir el título, etiquetas de los ejes y leyenda
  labs(title = "Curva ROC",
       x = "Porcentage de Falsos Positivos",
       y = "Porcentage de Falsos Negativos",
       color = "Modelo") +
  theme_bw() +
  theme(legend.position = "bottom")




NO2 +NO3 +PO4+ SiO2+Clorofila+Salinidad +pH+ OD+SST+Temperatura_mean+Salinidad_mean+Oxigeno_mean+Densidad_mean+PC01+PC02+PC03 )



library(ggplot2)


# Ajustar un modelo de regresión logística
modelo <- glm(vibrioData ~ NO2 +NO3 +PO4+ SiO2+Clorofila+Salinidad +pH+ OD+SST+Temperatura_mean+Salinidad_mean+Oxigeno_mean+Densidad_mean+PC01+PC02+PC03, family = binomial)
summary(modelo)
# Hacer predicciones para todos los valores de edad y colesterol en el dataframe
predicciones <- predict(modelo, newdata = data.frame(edad = rep(seq(40, 80, length.out = 100), each = 100),
                                                     colesterol = rep(seq(150, 250, length.out = 100), 100)))

# Crear un dataframe con los valores predichos y los valores de edad y colesterol correspondientes
predicciones_df <- data.frame(probabilidad = exp(predicciones) / (1 + exp(predicciones)),
                              edad = rep(seq(40, 80, length.out = 100), each = 100),
                              colesterol = rep(seq(150, 250, length.out = 100), 100))

# Hacer un plot de los datos y la frontera de decisión del modelo
ggplot(datos, aes(x = edad, y = colesterol, color = factor(enfermedad_cardiaca))) +
  geom_point() +
  geom_contour(aes(z = probabilidad), data = predicciones_df, bins = 10, color = "black", alpha = 0.5) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  theme_classic()
