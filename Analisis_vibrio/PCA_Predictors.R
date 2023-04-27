#Título del script: Análisis de Componentes principales, MRPP y Correlaciones de los datos fisicoquímicos.
#Autores: Christian Bermúdez-Rivas 
#Objetivo: Realizar los análisis de los datos fisico químicos.
#Lenguaje: R
#Fecha: Junio 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################



#library(Rcpp)
install.packages("factoextra")
library(gridExtra)
library(ggplot2)
library(factoextra)
library(corrplot)
library(usethis)
library(RColorBrewer)
vibrio=read.csv("VibrioTotal.csv")




#Correlación multiple
mul_correlacion<-stats::cor(na.omit(vibrio[,6:22]),  method = c("spearman") )
png(filename = "./Imagenes/corr_Matrix.png", res = 300, width = 12, height = 12, units = "cm", pointsize = 5, type = c("cairo"))

corrplot::corrplot(mul_correlacion, 
                   method = 'number',
                   type="upper", 
                   title=title,  
                   sig.level = 0.01, 
                   insig = "blank", 
                   col=brewer.pal(n=8, name="RdYlBu"), 
                   tl.col="black", tl.srt=45, mar=c(0,0,1,0) )
dev.off()

#Calcular los componentes principales
Pac_data_pca <- stats::prcomp(na.omit(vibrio[,6:22]), scale = TRUE)
print(Pac_data_pca)
summary(Pac_data_pca)
Pac_Comp_var<-as.data.frame(Pac_data_pca[5])
colnames(Pac_Comp_var)<-c("PC01", "PC02", 
                      "PC03", "PC04", 
                      "PC05", "PC06", 
                      "PC07","PC08", 
                      "PC09", "PC10", 
                      "PC11", "PC12", 
                      "PC13"
                      
)
#exportar los valores de los residuales del cáculo de los componentes
write.table(Pac_Comp_var, "Pac_PCA.csv", dec = ".", sep=",", row.names = FALSE)


######Gráficas de los componentes principales#####

Pac_var <-get_pca_var(Pac_data_pca)
#Gráfica de correlación entre lasvariables y los componentes
Pac_var_percentages<-as.data.frame(Pac_var$cos2)
colnames(Pac_var_percentages)<-c("PC01", "PC02", 
                      "PC03", "PC04", 
                      "PC05", "PC06", 
                      "PC07","PC08", 
                      "PC09", "PC10", 
                      "PC11", "PC12", 
                      "PC13"
                      
)
write.table(Pac_var_percentages, "aportes.csv", dec = ".", sep=",")

png(filename = "./Imagenes/Pac_PCA_Dim.png",width = 20, height = 20, units = "cm", res=300, pointsize = 0.1)
corrplot(Pac_var$cor, is.corr=TRUE)
dev.off()



Pac_graf01<-factoextra::fviz_eig(Pac_data_pca,addlabels = TRUE,hjust = -0.3,linecolor ="red")+
  labs(title="Screeplot",x="Dimensiones (PC)", y="% variable explicado.")+
  ylim(c(0,65))



Pac_PCA_12<-factoextra::fviz_pca_var(Pac_data_pca, 
                                    axes = c(1,2),
                                    
                                 col.var = "contrib",
                                 gradient.cols = c("#2c7fb8", "#ffeda0", "#f03b20"),
                                 ggtheme = theme_minimal())+
  labs(x="PC1 (40.7%)", y="PC2 (17.5%)")+ theme(plot.title = element_blank())

Pac_PCA_23<-factoextra::fviz_pca_var(Pac_data_pca, 
                                 axes = c(2,3),
                                 
                                 col.var = "contrib",
                                 gradient.cols = c("#2c7fb8", "#ffeda0", "#f03b20"),
                                 ggtheme = theme_minimal())+
  labs(x="PC2 (17.5%)", y="PC3 (8.8%)")+ theme(plot.title = element_blank())




png(filename = "./Imagenes/Pac_PCA_total.png",width = 20, height = 18, units = "cm", res=300, pointsize = 8)
grid.arrange(arrangeGrob(Pac_graf01),
             arrangeGrob(Pac_PCA_12, Pac_PCA_23, ncol = 2),
             ncol=1)
dev.off()

