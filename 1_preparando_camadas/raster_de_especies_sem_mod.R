##############################################
#### Gerar raster de espécies sem modelos ####
##############################################


library(rgeos)
library (raster)
setwd("D:/PRIM_estrada/especies_modelos")

# Carrega a tabela com todos os pontos das espécies sem modelos (Mayra)
sps_smod_ptos <- read.table("./sps_PRIM_estrada_ptos_sem_mod.txt", sep= "\t", header = T)

# Separa as espécies sem modelo
# sps_prim <- read.table("./sps_PRIM_estrada.txt", sep= "\t", header = T)
# sps_prim_smod <- subset(sps_prim,modelagem==0)
# Separa os pontos das espécies sem modelos.
# sps_prim_smod_merge <- merge(sps_ptos, sps_prim_smod, by.x="Taxon", by.y="TAXON" , all.x=T)
# sps_sem_mod <- subset(sps_prim_smod_merge,sps_prim_smod_merge$modelagem.x==0)

# Lista das espécies
sps <- as.vector(unique(sps_smod_ptos$Taxon))

# Carrega o raster 
ups_BR <- raster("./ups/raster/ups_BR.tif")


#sp = sps[1]

for (sp in sps){

sp_ptos <- subset(sps_smod_ptos, Taxon==sp)

sp_shp <- SpatialPointsDataFrame(coords = sp_ptos[,c(3,4)], data = sp_ptos , proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#plot(sp_shp,add=T)

pixel <- unique(extract(ups_BR,sp_shp))

pixel <- as.vector(na.exclude(pixel))

p = length(pixel)

while(p > 0) {
  
  r_up_sp_w <- ups_BR == pixel[p]
  
  if (p == length(pixel)) {r_up_sp = 0}
  
  r_up_sp <- r_up_sp + r_up_sp_w
  
  p = p-1
  
  #plot(r_up_sp)
  
  }

if (length(pixel)!=0){writeRaster(r_up_sp, paste0("./sem_mod/_poucos_pontos/",gsub(sp,pattern = " ", replacement = "_"),".tif" ), NAflag = -9999, format= "GTiff",overwrite=TRUE)
  }else {print(sp)}
}

# system('shutdown -h')


#### Confere espécies que estão sem raster ####

# Lista de espécies com modelos
sp_mod <- list.files("./bin/_1km", pattern = "\\.tif$")

# Lista de espécies sem mod (tiago)
sp_smod1 <- list.files("./bin/_sem_mod", pattern = "\\.tif$")

# Lista de espécies sem mod (Mayra)
sp_smod2 <- list.files("./sem_mod/_poucos_pontos", pattern = "\\.tif$")

# Espécies com raster
sp_raster <- c(sp_mod,sp_smod1,sp_smod2)
sp_raster <- gsub(pattern = "\\.tif$", replacement = "", sp_raster)
sp_raster <- gsub(pattern = "_", replacement = " ", sp_raster)
sp_raster <- sort(sp_raster)
sp_raster <- as.data.frame(sp_raster)
names(sp_raster) <- "TAXON"
sp_raster$raster <- 1

# Tabela todas as espécies
tab_sps <- read.table("./sps_PRIM_estrada.txt", sep="\t",header = T)

# Merge
tab_sraster <- merge(tab_sps,sp_raster, by.x="TAXON", by.y = "TAXON", all.x=T)
tab_sraster <- subset(tab_sraster, is.na(tab_sraster$raster))

#### Gera os raster das espécies que não tem modelos, nem pontos, mas tem polígonos

# Carrega o shape das espécies que só tem polígonos
sp_pol <- shapefile("./poligonos/2_select/sp_pol.shp")

# Carrega o raster 
ups_BR <- raster("./ups/raster/ups_BR.tif")

# Lista da sp
sps <- sp_pol$taxon

sp= sps[1]

for (sp in sps){
  
  # Carrega o shp da espécie
  sp_shp <- subset(sp_pol,sp_pol$taxon==sp)
  sp_shp@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Identifica o valor do pixel que tem interseção com o polígono
  pixel <- unique(extract(ups_BR,sp_shp))
  pixel <- unique(unlist(pixel, recursive = F, use.names = F))
  length(pixel)
  
  # Gerar o raster com base nos valores de pixel identificados
  r_up_sp <- ups_BR %in% pixel
  r_up_sp <- mask(r_up_sp,ups_BR)
  
  # Salva o raster gerado
  writeRaster(r_up_sp, paste0("./sem_mod/_poligonos/",gsub(sp,pattern = " ", replacement = "_"),".tif" ), NAflag = -9999, format= "GTiff",overwrite=TRUE)
  
}
