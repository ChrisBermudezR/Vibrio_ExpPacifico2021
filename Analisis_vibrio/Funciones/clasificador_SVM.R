
clasificador_SVM<-function(variable_1, variable_2, factor_1, nombre_1, nombre_2, kernel){
  
  
  grilla = expand.grid(variable_1 = variable_1, variable_2 = variable_2)
  ejex<-sort(grilla$variable_1)
  ejey<-sort(grilla$variable_2)
  
  
  summary(ejex)
  length(ejex)
  
  summary(ejey)
  length(ejey)
  
  ejex<-seq(from = min(ejex), to = max(ejex), length.out = 100)
  ejey<-seq(from = min(ejey), to = max(ejey), length.out = 100)
  
  
  
  x<-data.frame(variable_1, variable_2)
  plot(x, col = factor_1 + 1, xlab=nombre_1, ylab=nombre_2)
  dat = data.frame(factor_1 = factor(factor_1), x)
  fit = svm(factor(factor_1) ~ ., data = dat, scale = FALSE, kernel = kernel, cost = 5)
  print(fit)
  summary(fit)
  
  xgrid = expand.grid(variable_1 = ejex, variable_2 = ejey)
  ygrid = predict(fit, xgrid)
  
  
  plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2,xlab=nombre_1, ylab=nombre_2)
  points(x, col = factor_1 + 1, pch = 19)
  
  func = predict(fit, xgrid, decision.values = TRUE)
  func = attributes(func)$decision
  
  plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2,xlab=nombre_1, ylab=nombre_2)
  points(x, col = factor_1 + 1, pch = 19)
  
  contour(ejex, ejey, matrix(func, 100, 100), level = 0, add = TRUE)
  contour(ejex, ejey, matrix(func, 100, 100), level = 0.5, add = TRUE, col = "blue", lwd = 2)
}
