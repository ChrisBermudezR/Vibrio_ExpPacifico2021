install.packages("vegan")

library(vegan)

MRPP_PCA=read.csv("MRPP_PCA.csv")


Marea <- MRPP_PCA$Marea
Sector <- MRPP_PCA$Sector
Transecto <- MRPP_PCA$Transecto

mrpp_resultMarea <- mrpp(MRPP_PCA[, -1:-4], Marea)
mrpp_resultSector <- mrpp(MRPP_PCA[, -1:-4], Sector)
mrpp_resultTransecto <- mrpp(MRPP_PCA[, -1:-4], Transecto)
