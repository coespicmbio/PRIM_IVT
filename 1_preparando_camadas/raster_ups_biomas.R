######################################
#### Gerar raster de ups e biomas ####
######################################

library(rgeos)
library (raster)
setwd("D:/PRIM_estrada/especies_modelos")

# carrega shp das ups 
ups_shp <- shapefile("./ups/shp/ups_biomas.shp")

# carrega raster modelo
rmod <- raster("./limites/_LIMITES_0.tif")

# Salva a UPs do Brasil
ups_r_br <- rasterize(ups_shp,rmod, field=ups_shp$UP)
writeRaster(ups_r_br,paste0("./ups/raster/ups_BR.tif"), NAflag= -9999, format="GTiff", overwrite=T)
#b= unique(ups_shp$bioma)[2]

for (b in unique(ups_shp$bioma)){
  
  ups_shp_b <- subset(ups_shp,ups_shp$bioma==b)
  
  # Rasterize ups
  ups_r_b <- rasterize(ups_shp_b,rmod, field=ups_shp_b$UP)
  #plot(ups_r_b)
  writeRaster(ups_r_b,paste0("./ups/raster/",b,".tif"), NAflag= -9999, format="GTiff", overwrite=T)
  
  
  # Raster do bioma
  rb <- ups_r_b > 0
  rb <- mosaic(rb, rmod, fun=max)
  #plot(rb)
  writeRaster(rb,paste0("./limites/biomas/",b,".tif"), NAflag= -9999, format="GTiff", overwrite=T)
  
}
