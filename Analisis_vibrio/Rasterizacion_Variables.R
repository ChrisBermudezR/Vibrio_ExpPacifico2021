source("./Funciones/rasterizar_Variable.R")

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
rasterizar_Variable("Salinidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Salinidad, "Alta", "Salinidad", Datos_Raster)
rasterizar_Variable("Salinidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Salinidad, "Baja","Salinidad", Datos_Raster)

rasterizar_Variable("Temperatura", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Temperatura, "Alta", "Temperatura", Datos_Raster)
rasterizar_Variable("Temperatura", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Temperatura, "Baja","Temperatura", Datos_Raster)

rasterizar_Variable("DensidadFito", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$DensidadFito , "Alta", "DensidadFito", Datos_Raster)
rasterizar_Variable("DensidadFito", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$DensidadFito , "Baja","DensidadFito", Datos_Raster)

rasterizar_Variable("Clorofila", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Clorofila, "Alta", "Clorofila   ", Datos_Raster)
rasterizar_Variable("Clorofila", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Clorofila, "Baja","Clorofila   ", Datos_Raster)

rasterizar_Variable("NO2", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$NO2, "Alta", Exp_NO2, Datos_Raster)
rasterizar_Variable("NO2", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$NO2, "Baja",Exp_NO2, Datos_Raster)

rasterizar_Variable("NO3", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$NO3, "Alta", Exp_NO3, Datos_Raster)
rasterizar_Variable("NO3", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$NO3, "Baja",Exp_NO3, Datos_Raster)

rasterizar_Variable("PO4", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$PO4, "Alta", "PO4   ", Datos_Raster)
rasterizar_Variable("PO4", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$PO4, "Baja","PO4   ", Datos_Raster)


rasterizar_Variable("pH", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$pH, "Alta", "pH   ", Datos_Raster)
rasterizar_Variable("pH", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$pH, "Baja","pH   ", Datos_Raster)

rasterizar_Variable("OD", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$OD, "Alta", "OD   ", Datos_Raster)
rasterizar_Variable("OD", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$OD, "Baja","OD   ", Datos_Raster)


rasterizar_Variable("Transparencia", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Transparencia, "Alta", "Transparencia   ", Datos_Raster)
rasterizar_Variable("Transparencia", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Transparencia, "Baja","Transparencia   ", Datos_Raster)

rasterizar_Variable("SST", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$SST, "Alta", "SST   ", Datos_Raster)
rasterizar_Variable("SST", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$SST, "Baja","SST   ", Datos_Raster)

rasterizar_Variable("Densidad", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$Densidad, "Alta", "Densidad   ", Datos_Raster)
rasterizar_Variable("Densidad", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$Densidad, "Baja","Densidad   ", Datos_Raster)

rasterizar_Variable("DensidadFito", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$DensidadFito, "Alta", "DensidadFito   ", Datos_Raster)
rasterizar_Variable("DensidadFito", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$DensidadFito, "Baja","DensidadFito   ", Datos_Raster)

rasterizar_Variable("PesoHum500", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$PesoHum500, "Alta", "PesoHum500   ", Datos_Raster)
rasterizar_Variable("PesoHum500", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$PesoHum500, "Baja","PesoHum500   ", Datos_Raster)


rasterizar_Variable("PesoHum300", Datos_Raster_alta$Longitude, Datos_Raster_alta$Latitude, Datos_Raster_alta$PesoHum300, "Alta", "PesoHum300   ", Datos_Raster)
rasterizar_Variable("PesoHum300", Datos_Raster_baja$Longitude, Datos_Raster_baja$Latitude, Datos_Raster_baja$PesoHum300, "Baja","PesoHum300   ", Datos_Raster)

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
