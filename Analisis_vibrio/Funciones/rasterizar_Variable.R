 rasterizar_Variable<-function(nombre_variable,longitud, latitud, variable, marea, leyenda, datos){
  assign("WIRE",interpBarnes(longitud, latitud, variable), envir = parent.frame())
  assign(paste0("pts.grid"),expand.grid(Longitud=WIRE$x, Latitud=WIRE$y), envir = parent.frame())
  assign(paste0("pts.grid"), mutate(pts.grid, variable=as.vector(WIRE$zg)), envir = parent.frame())
  assign(paste0("export"),  raster::rasterFromXYZ(pts.grid), envir = parent.frame())
  assign(paste0(nombre_variable,"_",marea,"_pts.grid"),  rasterFromXYZ(pts.grid), envir = parent.frame())
  raster::writeRaster(export, filename=paste("./SIG_Datos/grids/",nombre_variable,"_",marea, ".tif", sep = ""),overwrite=TRUE)
  
  
assign(paste0(nombre_variable,"_",marea,"_plot"), ggplot(pts.grid, aes(Longitud, Latitud)) +
    geom_raster(aes(fill = variable))+
    geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#3aaa05", fill="#3aaa05") +
    geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
    coord_sf(xlim = c(-78.405, -78.216), ylim = c(2.621, 2.855), expand = FALSE)+   
    geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent") +
    theme_bw()+
    scale_fill_gradientn(colours = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"))+
    geom_point(data=estaciones, aes(x= Longitud, y= Latitud_De))+
    labs(fill=leyenda, title= paste(nombre_variable,"-",marea))
    #ggrepel::geom_text_repel(data=marea_alta,aes(x=longitud, y=latitud,label = Estacion),box.padding   = 0.3, direction = "x")
, envir = parent.frame())
}