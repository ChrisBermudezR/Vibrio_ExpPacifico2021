install.packages("vegan")

library(vegan)

MRPP_PCA=read.csv("Datos_Raster.csv")

MRPP_PCA$MareaFactor<-as.factor(MRPP_PCA$MareaFactor)
Marea <- MRPP_PCA$MareaFactor
Sector <- MRPP_PCA$Sector
Transecto <- MRPP_PCA$Transecto

mrpp_resultMarea <- mrpp(MRPP_PCA[, -1:-5], Marea)
capture.output("Resultados MRPP MArea",
               mrpp_resultMarea,
               file="Resultados_MRPP.txt"
  
)
