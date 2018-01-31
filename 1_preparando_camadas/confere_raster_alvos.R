##########################################################################################################################
#### Confere se os rasters dos alvos não são vazios e estão corretamente alocados conforme o Feature List do Zonation ####
##########################################################################################################################

library(raster)

# Define diretório de trabalho
setwd("D:/PRIM_estrada/_analise_priorizacao/mata_atlantica")

# Carrega o raster de unidades de planejamento
up <- raster("./unidade_planejamento/raster_up_mata.tif")

# Carrega o feature list
flist <- read.table(file= "./inputs/feature_list_18_01.txt", header = F, sep = "\t")
flist <- as.vector(flist[,6])
flist <- gsub(pattern = "mata_atlantica/",replacement = "",flist)


# Cria a tabela vazia que receberá os dados da conferência
tab_check <- data.frame("alvo"=character(0),"existencia"=logical(0),"extensao"= logical(0),
                        "n_colunas"= logical(0), "n_linhas"= logical(0),"resolucao"= logical(0),
                        "valores"= logical(0))
#f<- flist[1]

# Realiza a conferência
# Progresso do for loop
p <- 1
inicio <- date ()
for (f in flist){
  
  # Nome do alvo
  nome <- basename(f)
  nome <- gsub(pattern = "\\.tif$",replacement = "",nome)
  
  # Confere se o raster existe
  existe <- file.exists(f)
  
  if (existe==TRUE){
  # Carrega o raster
  r <- raster(f)
  
  # Confere se up e raster possuem os mesmos parâmetros
  extensao <- r@extent == up@extent
  colunas <- r@ncols == up@ncols
  linhas <- r@nrows == up@nrows
  resolucao <- TRUE %in% as.vector(res(r) == res(up))
  
  # Confere se o raster não está vazio
  valores <- cellStats(r,stat="sum",na.rm=T)>0
  }# Fecha Condicional de existencia do raster
  if (existe==FALSE){
  extensao <- FALSE
  colunas <- FALSE
  linhas <- FALSE
  resolucao <- FALSE
  valores <- FALSE
  }# Fecha Condicional de não existencia do raster
  
  # Cria a tabela de conferência do alvo
  tab_r <- data.frame("alvo"= nome,"existencia"=existe,"extensao"= extensao,
                      "n_colunas"= colunas, "n_linhas"= linhas,"resolucao"= resolucao,
                      "valores"= valores)
  
  # Adiciona à tabela final
  tab_check <- rbind(tab_check,tab_r)
  
  # Progresso do for loop
  cat("Progresso",p,"de", length(flist),"<==>",round((p/length(flist)*100),2),"%","__ Início:",inicio,"__ Atual:", date() ,"\n")
  
  # Progresso
  p <- p + 1
  
} # Fecha For Loop

# Salva a tabela de conferência de todos os alvos
write.table(tab_check,file="./tab_check_alvos.txt",quote=F,sep="\t",row.names = F,overwrite=T)


