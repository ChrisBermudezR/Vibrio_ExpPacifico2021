modelosEval_Factor<-function(variable, nombre_variable, ejex, Factor, nombre_factor){
  
  png(filename = paste0("./Imagenes/",nombre_factor,nombre_variable,"_boxplot.png"), width = 15, height = 15, units = "cm", pointsize = 15, res = 300)
  plot(variable~factor(Factor), xlab=nombre_factor, ylab=ejex, pch=20)
  dev.off()
  
  glm.fit=glm(Factor  ~ variable, family=binomial)
  resultados<-summary(glm.fit)
  capture.output(nombre_variable, glm.fit,resultados, file = paste0("./Resultados/",nombre_factor,nombre_variable,"_glm.txt"))
  
  
  png(filename = paste0("./Imagenes/",nombre_factor,nombre_variable,"_glm.png"), width = 15, height = 15, units = "cm", pointsize = 15, res = 300)
  plot(x=variable, y=Factor, xlab=ejex,
       ylab="Probabilidad estimada", panel.first=grid (col="gray", lty="dotted"), pch=20)
  
  lines(seq(min(variable),max(variable), length.out=24),
        predict(glm.fit, newdata=data.frame(sq=seq(min(variable),max(variable), length.out=24)),
                type="response"), col="red", lwd=2) 
  dev.off()
  
  rf.model <- randomForest(factor(Factor) ~ variable)
  capture.output(nombre_variable, rf.model, file = paste0("./Resultados/",nombre_factor,nombre_variable,"_rf.txt"))
  
  
  png(filename = paste0("./Imagenes/",nombre_factor,nombre_variable,"_ROC.png"), width = 15, height = 15, units = "cm", pointsize = 15, res = 300)
  roc(Factor, 
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
  
  plot.roc(Factor, 
           rf.model$votes[,1],
           percent=TRUE, 
           col="#4daf4a", 
           lwd=4, 
           print.auc=TRUE, 
           add=TRUE, 
           print.auc.y=44,
           print.auc.x=45,)
  
  legend("bottomright", legend=c("Regr. Logistica", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)
  par(pty = "m")
  dev.off()
}

