###########################################################
#### Cortar as camadas pela interseção da camada comum ####
###########################################################

library(rgeos)
library (raster)
setwd("D:/PRIM_estrada/_analise_priorizacao")

# Raster de máscara do bioma (limite) (modelo)
cer_lim <- raster("cerrado/unidade_planejamento/raster_up_cerrado.tif")
crs(cer_lim) <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Troca valores zero por NA
unique(cer_lim==0)
cer_lim[cer_lim==0] <- NA

# Salva o raster da UP sem 0 
writeRaster(cer_lim,"cerrado/unidade_planejamento/raster_up_cerrado.tif", NAflag= -9999, overwrite=T)

# Raster para ponto
# Gera o arquivo de pontos
cer_pto <- rasterToPoints(cer_lim,spatial = T)
head(cer_pto@data)
crs(cer_pto) <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#shapefile(ca_pto,"ca_teste.shp")

# Troca valores NA's por zero
#r[is.na(r[])] <- 0 

# Amazônia
#ameaca <- list.files("./amazonia/alvos_conservacao/ameacas", pattern = "\\.tif$", full.names = T)
#fauna <- list.files("./amazonia/alvos_conservacao/especies_fauna", pattern = "\\.tif$", full.names = T)
#flora <- list.files("./amazonia/alvos_conservacao/especies_flora", pattern = "\\.tif$", full.names = T)
#habitat <- list.files("./amazonia/alvos_conservacao/habitats_especificos", pattern = "\\.tif$", full.names = T)
#condicao <- list.files("./amazonia/condicao_paisagem", pattern = "\\.tif$", full.names = T)
#mascara <- list.files("./amazonia/mascara", pattern = "\\.tif$", full.names = T)
#ups <- list.files("./amazonia/unidade_planejamento", pattern = "\\.tif$", full.names = T)

# Camadas do bioma
camadas <- list.files("./cerrado", pattern = "\\.tif$", full.names = T, recursive = T)
#camadas <- camadas[2:1075]
#tail(camadas)
#camadas <- camadas[7:168]
#rf <- camadas[1]

# Progresso
p <- 1

for (rf in camadas){
  
  cat(">>>",gsub(pattern="\\.tif$",replacement="",basename(rf)), "\n")
  cat(paste("progresso: ",round(((p/length(camadas)))*100,2),"%"),"\n")
  cat(paste( p,"de",length(camadas)), "\n")
  
  # Carrega o raster
  r <- raster(rf)
  #plot(r)
  
  cat ("> gerando shp de pontos...", "\n")
  
  # Gera o arquivo de pontos
  #r_pto <- rasterToPoints(r,spatial = T)
  
  # Extrai os valores dos pixel dos modelos
  valores <- extract(r,cer_pto)
  valores[is.na(valores)] <- 0
  #unique(is.na(valores))
  cer_pto$pixel <- valores
  #head(cer_pto@data)
  
  cat ("> rasterizando...", "\n")
  
  # Rasterizar o shape de ptos
  #field <- names(r_pto)
  r_r <- rasterize(cer_pto,cer_lim, field="pixel")
  #r_r
  #plot(r_r)
  
  #cat ("> cortando...", "\n")
  
  # Faz o mask
  #r_r <- mask(r_r,ca_lim)
  
  # Iguala à extensão do raster modelo
  #r <- extend(r,am_lim, value=0)
  #origin(r) <- origin(am_lim)
  
  # Interseção
  #r_int <- r_r*am_lim
  
  #plot(am_lim)
  #plot(r_int)
  
  cat ("> salvando...", "\n")
  
  # Salva o raster com extensão ajustada
  writeRaster(r_r,rf, NAflag= -9999, overwrite=T)
  
  # Progresso
  p <- p + 1
  
  cat("===================","\n")
}

system('shutdown -h')
