library(rgeos)
library (raster)
setwd("D:/PRIM_estrada/especies_modelos")

#### Gerar o Raster de UCs ####
# Carregar o limite das UCs
#ucs <- shapefile("./ucs/UCS_FEM.shp")

# Carregar um raster modelo
rmod <- raster("./limites/_LIMITES_0.tif")
plot(rmod)
# Extrai a posição dos pixel células que tem interseção com o shape
#n_cel <- cellFromPolygon(rmod, ucs, weights=T)
#n_cel <- as.data.frame(n_cel)
#n_cel <- n_cel$cell

# Transforma o numero de posicao da celula em um shapefile de pontos
#pto_r <- xyFromCell(rmod, n_cel, spatial=T)

# Rasterize
#uc_r <- rasterize(pto_r, rmod, field=1, fun='last', background=-9999)

# Salva raster de UC
#writeRaster(uc_r,"./ucs/UCS_FEM.tif", NAflag= -9999, overwrite=T)

# Carrega o raster de UCs
ucs <- raster("./ucs/UCS_FEM.tif")

#### Rasters dos biomas ####
# Listar arquivos de raster de ups
ups_f <- list.files("./ups", pattern = "\\.tif$", full.names = T)


# Carrega o raster dos biomas
biomas <- raster("./limites/_LIMITES_1_6.tif")
nomes <- c("am","ca", "ce", "ma", "pp","pt")
itens <- c(1:6)

for (b in itens) {
  
  # carregar o raster de ups do bioma
  up_b <- biomas==b
  
  # Salva rasters do bioma
  nome <- nomes [b]
  writeRaster(up_b,paste0("./limites/biomas/",nome), NAflag= -9999, format="GTiff", overwrite=T)
  
  cat(b, "\n")
}


#### Cálculo da proporção em UC e biomas ####

# Tabela vazia para receber valores
#tab_prop <- data.frame("taxon"=character(0),"prop_UC"=numeric(0),
                    #   "prop_UC"=0,"prop_pt"= 0, "prop_pp"=0, "prop_ma"=0,"prop_ce"=0,"prop_ca"=0,"prop_am"=0)
tab_props <- data.frame("prop_UC"=numeric(0),"prop_pt"= numeric(0), "prop_pp"=numeric(0), "prop_ma"=numeric(0),
                        "prop_ce"=numeric(0),"prop_ca"=numeric(0),"prop_am"=numeric(0))

# Cria os vetores 
taxon <- character()

# Carrega os rasters binários das espécies da fauna
spsF <- list.files("./bin/_1km_todas", pattern = "\\.tif$", full.names = T)
#sp = spsF[1]
#plot(raster(sp))

# Progresso
p <- 1

for (sp in spsF){
  
  cat(">>>",gsub(pattern="\\.tif$",replacement="",basename(sp)), "\n")
  cat(paste0("progresso: ",round((p/length(spsF)),2)*100,"%", "\n"))
  
  # Cria vetor vazio
  props <- numeric()
  
  # Adiciona o nome da espécie ao vetor
  taxon <- append(x= taxon, values =gsub(pattern="\\.tif$",replacement="",basename(sp)))
  
  # Linha a ser adicionada à tabela vazia
  #linha <- data.frame("taxon"= gsub(pattern="\\.tif$",replacement="",basename(sp)),
                  #    "prop_UC"=0,"prop_pt"= 0, "prop_pp"=0, "prop_ma"=0,"prop_ce"=0,"prop_ca"=0,"prop_am"=0)
  
  #linha <- data.frame("taxon"=character(0))
  #linha$taxon <- gsub(pattern="\\.tif$",replacement="",basename(sp))
  
  # Carrega o raster da espécie
  spR <- raster(sp)
  
  cat("raster carregado", "\n")
  
  # Calcula o total de células que a espécie ocupa
  sp_ncel <- cellStats(spR, stat='sum', na.rm=TRUE)
  
  # Calcula a proporção das células da espécie que ocorre em UC
  spXuc <- spR * ucs
  spXuc_ncel <- cellStats(spXuc, stat='sum', na.rm=TRUE)
  spXuc_prop <- round(spXuc_ncel/sp_ncel,2)
  props <- append(x = props, values= spXuc_prop)
  
  cat("prop em UC calculada", "\n")
  
  # Proporção de UC na Linha
  #linha <- data.frame("taxon"= gsub(pattern="\\.tif$",replacement="",basename(sp)),"prop_UC"=spXuc_prop,
                   #   "prop_pt"= 0, "prop_pp"=0, "prop_ma"=0,"prop_ce"=0,"prop_ca"=0,"prop_am"=0)
  
  # Calcula a proporção das células da espécie que ocorre em biomas
  # Lista de rasters dos biomas
  rbF <- list.files("./limites/biomas", pattern = "\\.tif$", full.names = T)
  n_biomas <- length(rbF)
  
  while (n_biomas>0){
    # Carrega o raster do bioma
    rb <- raster(rbF[n_biomas])
    
    # Calcula a proporção
    spXbioma <- spR * rb
    spXbioma_ncel <- cellStats(spXbioma, stat='sum', na.rm=TRUE)
    prop_bioma <- spXbioma_ncel/sp_ncel
    props <- append(x = props, values= prop_bioma)
    
    # Nome do bioma
    #nomeBioma <- names(rb)
    cat(paste0("prop do ", names(rb)," calculada"), "\n")
    
    
    # Prepara para o próximo bioma
    n_biomas <- n_biomas-1
    
    # Progresso
    p <- p +1
  }
  cat("===================","\n")
  # Adiciona a
  tab_props <- rbind.data.frame(tab_props,props)
  names(tab_props) <- c("prop_UC","prop_pt", "prop_pp", "prop_ma","prop_ce","prop_ca","prop_am")
}

# Constroi tabela final
tab_final <- cbind(taxon, tab_props)

# Salva tabela final
write.table(tab_final,"./tabela_proporcoes_fauna.txt", sep = "\t", row.names = F, quote = F)





