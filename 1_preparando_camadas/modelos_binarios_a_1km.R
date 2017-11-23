
#### Gerar modelos binários e contínuos a 1km  (PEIXES) ####

library(rgeos)
library (raster)
setwd("D:/PRIM_estrada/especies_modelos")

# Lista modelos contínuos de 10 km
mod_10 <- list.files("./cont/_10km/_peixes",pattern="\\.tif$", full.names = T)

# Raster modelo
r_mod <- raster("./cont/_1km/Aburria_jacutinga.tif")

# Converte para 1 km no limite do Brasil
for (r in mod_10) {
  
  mod <- raster(r)
  
  mod_1km <- resample(mod,r_mod)
  
  mod_1km <- mask(mod_1km,r_mod)
  
  writeRaster(mod_1km,paste0("./cont/_1km/_peixes/",basename(r)), NAflag= -9999, overwrite=T)
  
  cat(basename(r), "\n")
}

# Lista modelos contínuos de 1 km
mod_1km <- list.files("./cont/_1km/_peixes",pattern="\\.tif$", full.names = T)

# Converte para binário
for (r in mod_1km) {
  
  mod <- raster(r)
  
  valores <- unique(mod)
  
  valores <- valores[valores != 0]
  
  limiar <- min(valores)
  
  mod_bin <- mod > limiar
  
  writeRaster(mod_bin,paste0("./bin/_1km_peixes/",basename(r)), NAflag= -9999, overwrite=T)
  
  cat(basename(r), "\n")
}


#system('shutdown -h')
