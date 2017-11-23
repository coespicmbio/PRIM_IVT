####################################
#### Gerando raster de cavernas ####
####################################

library (raster)
library(maptools)
setwd("D:/PRIM_estrada/cavernas")

# carregar raster modelo
raster_mod <- raster ("./grade/limite/_LIMITES.tif")
plot(raster_mod)

# carrega o shp de limite BR
limite <- shapefile("./grade/limite/_LIMITES.shp")
plot(limite,add=T)

# Corta o raster modelo pelo limite
r_crop <- crop(raster_mod,limite)
r_masked <- mask(r_crop, limite)
plot(r_masked)

# Salva
writeRaster(r_masked,"./grade/limite/_LIMITES_1_6.TIF", NAflag=-9999, overwrite=T)

r_0 <- r_masked  - r_masked
plot(r_0)

# Salva
writeRaster(r_0,"./grade/limite/_LIMITES_0.TIF", NAflag=-9999, overwrite=T)

#############################

r_0 <- raster("./grade/limite/_LIMITES_0.TIF")

# Lista as classes de rochas por bioma
lista_rochas <- list.files("./classes_rochas/1_split/pantanal", pattern ="\\.shp$", full.names = T)

# Shapefile do bioma
bioma <- readShapePoly(fn= "./grade/limite/pantanal.shp",proj4string=CRS("+proj=longlat +datum=WGS84"))

# Carrega o raster de riqueza de cavernas do bioma
riqueza <- raster ("./classes_rochas/2_spjoin/riqueza/pantanal.tif")
plot(riqueza)

for (r in lista_rochas){
  
  # Carrega as classes de rochas
  rochas <- readShapePoly(fn= r,proj4string=CRS("+proj=longlat +datum=WGS84"))
  cat ( basename(r), "\n")

  # Identifica a posição das celulas
  cat ( "posicao", "\n")
  n_cel <- cellFromPolygon(r_0, rochas, weights=T)
  n_cel <- as.data.frame(n_cel)
  n_cel <- n_cel$cell
  
  # Transforma o numero de posicao da celula em um shapefile de pontos
  cat ( "posicao para pto", "\n")
  pto_r <- xyFromCell(r_0, n_cel, spatial=T)
  
  # Rasterize
  cat ( "rasterize", "\n")
  r_rochas <- rasterize(pto_r,r_0, field = 0, background = -1)
  #r_rochas
  r_rochas <- r_rochas + 1
  r_rochas@crs <- rochas@proj4string
  cat ( "mask", "\n")
  r_rochas <- crop(r_rochas,bioma)
  r_rochas <- mask(r_rochas,bioma)
 
  
  # Soma com a camada de riqueza
  cat ( "soma riqueza", "\n")
  riqueza@crs <- rochas@proj4string
  riqueza_inter <- r_rochas * riqueza
  r_rochas_riq <- r_rochas + riqueza_inter
  
  # Variar 0 a 1
  cat ( "variar 0 a 1", "\n")
  r_rochas_riq <- (r_rochas_riq-minValue(r_rochas_riq))/(maxValue(r_rochas_riq)-minValue(r_rochas_riq))
  #plot(r_rochas_riq)
  #plot(r_rochas)
  #plot(rochas,add=T)
  nome <- basename(r)
  nome <- gsub(nome, pattern = ".shp", replacement = "")
  
  # Salva
  cat ( "salvando", "\n")
  writeRaster(r_rochas_riq,paste0("./classes_rochas/3_raster/pantanal/",nome, ".tif" ), NAflag=-9999, overwrite=T)
  cat ( "=========", "\n")
  
}







 
 
 
 
 
 
 



