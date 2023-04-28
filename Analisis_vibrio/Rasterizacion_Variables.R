source("./Funciones/rasterizar_Variable.R")

library(oce)
Datos_Raster=read.csv("Datos_Raster.csv")




costa<-rgdal::readOGR("./SIG_Datos/costa.shp")
rios<-readOGR("./SIG_Datos/rios_wgs84.shp")
estaciones<-readOGR("./SIG_Datos/estaciones.shp")
areas_protegidas<-readOGR("./SIG_Datos/areas_protegidas.shp")
estaciones<-as.data.frame(estaciones)

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

  
Datos_Raster$MareaFactor
Datos_Raster_alta<-  subset(Datos_Raster,MareaFactor == "1")
Datos_Raster_baja<-  subset(Datos_Raster,MareaFactor == "0")

source("./Funciones/rasterizar_Variable.R")
#rasterizar_Variable(nombre_variable,longitud, latitud, variable, marea, leyenda)
rasterizar_Variable("Salinidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Salinidad, "Alta", Exp_Salinidad, Datos_Raster)
rasterizar_Variable("Salinidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Salinidad, "Baja",Exp_Salinidad, Datos_Raster)

rasterizar_Variable("Temperatura", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Temperatura, "Alta", Exp_Temperatura, Datos_Raster)
rasterizar_Variable("Temperatura", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Temperatura, "Baja",Exp_Temperatura, Datos_Raster)

rasterizar_Variable("DensidadFito", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$DensidadFito , "Alta", Exp_DensidadFito, Datos_Raster)
rasterizar_Variable("DensidadFito", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$DensidadFito , "Baja",Exp_DensidadFito, Datos_Raster)

rasterizar_Variable("Clorofila", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Clorofila, "Alta", Exp_Clorofila, Datos_Raster)
rasterizar_Variable("Clorofila", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Clorofila, "Baja",Exp_Clorofila, Datos_Raster)

rasterizar_Variable("NO2", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$NO2, "Alta", Exp_NO2, Datos_Raster)
rasterizar_Variable("NO2", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$NO2, "Baja",Exp_NO2, Datos_Raster)

rasterizar_Variable("NO3", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$NO3, "Alta", Exp_NO3, Datos_Raster)
rasterizar_Variable("NO3", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$NO3, "Baja",Exp_NO3, Datos_Raster)

rasterizar_Variable("PO4", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$PO4, "Alta", Exp_PO4, Datos_Raster)
rasterizar_Variable("PO4", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$PO4, "Baja",Exp_PO4, Datos_Raster)

rasterizar_Variable("SiO2", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$PO4, "Alta", Exp_SiO2, Datos_Raster)
rasterizar_Variable("SiO2", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$PO4, "Baja",Exp_SiO2, Datos_Raster)


rasterizar_Variable("pH", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$pH, "Alta", "pH   ", Datos_Raster)
rasterizar_Variable("pH", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$pH, "Baja","pH   ", Datos_Raster)

rasterizar_Variable("OD", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$OD, "Alta", Exp_OD, Datos_Raster)
rasterizar_Variable("OD", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$OD, "Baja",Exp_OD, Datos_Raster)


rasterizar_Variable("Transparencia", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Transparencia, "Alta", Exp_Transparencia, Datos_Raster)
rasterizar_Variable("Transparencia", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Transparencia, "Baja",Exp_Transparencia, Datos_Raster)

rasterizar_Variable("SST", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$SST, "Alta", Exp_SST, Datos_Raster)
rasterizar_Variable("SST", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$SST, "Baja",Exp_SST, Datos_Raster)

rasterizar_Variable("Densidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Densidad, "Alta", Exp_Densidad, Datos_Raster)
rasterizar_Variable("Densidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Densidad, "Baja",Exp_Densidad, Datos_Raster)

rasterizar_Variable("DensidadFito", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$DensidadFito, "Alta", Exp_DensidadFito, Datos_Raster)
rasterizar_Variable("DensidadFito", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$DensidadFito, "Baja",Exp_DensidadFito, Datos_Raster)

rasterizar_Variable("PesoHum500", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$PesoHum500, "Alta", Exp_PesoHum500, Datos_Raster)
rasterizar_Variable("PesoHum500", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$PesoHum500, "Baja",Exp_PesoHum500, Datos_Raster)


rasterizar_Variable("PesoHum300", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$PesoHum300, "Alta", Exp_PesoHum300, Datos_Raster)
rasterizar_Variable("PesoHum300", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$PesoHum300, "Baja",Exp_PesoHum300, Datos_Raster)

rasterizar_Variable("q0", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$q0, "Alta", "q0   ", Datos_Raster)
rasterizar_Variable("q0", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$q0, "Baja","q0   ", Datos_Raster)

rasterizar_Variable("q1", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$q1, "Alta", "q1   ", Datos_Raster)
rasterizar_Variable("q1", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$q1, "Baja","q1   ", Datos_Raster)




png(filename = "./Imagenes/01_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( NO2_Alta_plot, 
           NO2_Baja_plot,
           NO3_Alta_plot, 
           NO3_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()


png(filename = "./Imagenes/02_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( PO4_Alta_plot, 
           PO4_Baja_plot,
           SiO2_Alta_plot, 
           SiO2_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()




png(filename = "./Imagenes/03_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( Clorofila_Alta_plot, 
           Clorofila_Baja_plot,
           pH_Alta_plot, 
           pH_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()

png(filename = "./Imagenes/04_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( OD_Alta_plot, 
           OD_Baja_plot,
           Transparencia_Alta_plot, 
           Transparencia_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()


png(filename = "./Imagenes/05_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( SST_Alta_plot, 
           SST_Baja_plot,
           Temperatura_Alta_plot, 
           Temperatura_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()



png(filename = "./Imagenes/06_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( Salinidad_Alta_plot, 
           Salinidad_Baja_plot,
           Densidad_Alta_plot, 
           Densidad_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()

png(filename = "./Imagenes/07_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( DensidadFito_Alta_plot, 
           DensidadFito_Baja_plot,
           PesoHum500_Alta_plot, 
           PesoHum500_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()

png(filename = "./Imagenes/08_Variables.png", res = 300, width = 20, height = 20, units = "cm", pointsize = 13)

ggarrange( PesoHum300_Alta_plot, 
           PesoHum300_Baja_plot,
           q0_Alta_plot, 
           q0_Baja_plot, 
           ncol =  2, nrow = 2,legend ="bottom")
dev.off()

png(filename = "./Imagenes/09_Variables.png", res = 300, width = 20, height = 10, units = "cm", pointsize = 13)

ggarrange( 
           q1_Alta_plot, 
           q1_Baja_plot, 
           ncol =  2, nrow = 1,legend ="bottom")
dev.off()
