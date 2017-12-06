##########################
#### Mapas bivariados ####
##########################

# Carregar bibliotecas
library(raster)
library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)
library(maptools)


# Configurar diretório de trabalho
setwd("D:/PRIM_estrada/_analise_priorizacao/mata_atlantica")

#### Contruindo tabela de dados ####

## Carrega raster das Unidades de planejamento
up <- raster("unidade_planejamento/raster_up_mata.tif")

# Transforma o raster de UPs em polígonos
up_shp <- rasterToPolygons(up, dissolve=T)

## Carrega o resultado do Zonation (importância biológica)
rank <- raster("mata_11_21/outputs/mata_11_21_rank.tif")

## Carrega as ameaças
# Proporção da paisagem (perda de habitat)
prop_paisagem <- raster("ameacas/r_MT_prop_landscape.tif")

# Tamanho efetivo da malha (fragmentação)
mesh_size <- raster("ameacas/r_MT_effective_mesh_size_inverso.tif")

# Risco de colisão
colisao <- raster("ameacas/risco_colisao_mata.tif")

## Extrai os valores (isso pode demorar algumas horas ou um dia inteiro dependendo do tamanho do raster)
# Resultado do Zonation
up_values <- extract(rank, up_shp, fun=max, na.rm=T,sp=T)

# Tamanho efetivo da malha
up_values <- extract(mesh_size, up_values, fun=max, na.rm=T, sp=T)

# Proporção da paisagem
up_values <- extract(prop_paisagem, up_values, fun=max, na.rm=T,sp=T)

# Risco de colisão
up_values <- extract(colisao, up_values, fun=mean, na.rm=T,sp=T)

# Calcula uma média dos três valores de ameaças
up_values$mean <- (up_values$r_MT_effective_mesh_size_inverso + up_values$r_MT_prop_landscape + up_values$risco_colisao_mata)/3

# Reconstroi o raster de risco de colisao para se ter uma média por ottobacias
colisao_mean <- rasterize(up_values,colisao,field="mean")

## Transfere a tabela de atributos para um data frame simples
dados <- up_values@data
names(dados) <- c("up","zonation","mesh_size","prop_land", "colisao","mean")

# Mostra a estrutura e sumário dos dados
str(dados)
summary(dados)

#### ####

#### Avaliar a autocorrelação entre as variáveis ####

## Funções para construção dos gráficos de correlação

#
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

#
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

## Plota, em uma janela independente, os gráficos de correlação, histogramas
windows(70,60)
pairs(dados[complete.cases(dados), 2:5],
      lower.panel = panel.cor, diag.panel = panel.hist,
      cex.labels=1, font.labels = 2, cex = 1, pch = 21, bg = "light blue")
cor(dados[,c(2:5)])



#### Mapas bivairados e gráficos ####

### ATENÇÃO: o número de quantiles deve ser definido como argumento ("nquantiles") das funções "colmat" e "bivariate.map"

### Função para definir o gradiente de cores a serem usados nos mapas e gráficos bivariados ####
colmat<-function(nquantiles=4, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){
                my.data<-seq(0,1,.01)
                my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
                my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
                my.pal.2<-findColours(my.class,c(upperright, bottomright))
                col.matrix<-matrix(nrow = 101, ncol = 101, NA)
                for(i in 1:101){
                    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
                    col.matrix[102-i,]<-findColours(my.class,my.col)}
                    plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
                for(i in 1:101){
                    col.temp<-col.matrix[i-1,]
                    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)}
                    seqs<-seq(0,100,(100/nquantiles))
                    seqs[1]<-1
                    col.matrix <- col.matrix[c(seqs), c(seqs)]}

# Mostra os arguemntos da função "colmat" que podem ser alterados, como por exemplo, cores.
args(colmat)

col.matrix <- colmat(nquantiles=4)


### Função do mapa bivariado ####
bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=4){
                        quanmean<-getValues(rasterx)
                        temp <- data.frame(quanmean, quantile=rep(NA, length(quanmean)))
                        brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
                        r1 <- within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
                        quantr<-data.frame(r1[,2]) 
                        quanvar<-getValues(rastery)
                        temp <- data.frame(quanvar, quantile=rep(NA, length(quanvar)))
                        brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
                        r2 <- within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
                        quantr2<-data.frame(r2[,2])
                        as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
                        col.matrix2<-colormatrix
                        cn<-unique(colormatrix)
                        for(i in 1:length(col.matrix2)){
                            ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
                            cols<-numeric(length(quantr[,1]))
                        for(i in 1:length(quantr[,1])){
                            a<-as.numeric.factor(quantr[i,1])
                            b<-as.numeric.factor(quantr2[i,1])
                            cols[i]<-as.numeric(col.matrix2[b,a])}
                            r<-rasterx
                            r[1:length(r)]<-cols
                            return(r)}


#### Mapa bivariado PERDA X BIOLOGICO ####
# Legenda
windows(5,5)
col.matrix.perda <- colmat(nquantiles=4, xlab="Perda de habitat", ylab="Valor Biológico")
# Mapa bivariado. Ajustar o tamanho adequado da janela.
bivmap.perda<-bivariate.map(prop_paisagem,rank, colormatrix=col.matrix.perda, nquantiles=4)
windows(70,60)
plot(bivmap.perda,frame.plot=F,axes=F,box=F,add=F,legend=F,col=as.vector(col.matrix.perda))
###Salvar pela janela do grafico: arquivo-> salvar como


#### Mapa bivariado Risco de morte por COLISÃO X BIOLOGICO ####
# Legenda
windows(5,5)
col.matrix.colisao <- colmat(nquantiles=4, xlab="Risco de morte por colisão", ylab="Valor Biológico")
# Mapa bivariado COLISAO. Ajustar o tamanho adequado da janela.
bivmap.colisao<-bivariate.map(colisao_mean,rank, colormatrix=col.matrix.colisao, nquantiles=4)
windows(70,60)
plot(bivmap.colisao,frame.plot=F,axes=F,box=F,add=F,legend=F,col=as.vector(col.matrix.colisao))
### Salvar pela janela do grafico: arquivo-> salvar como


#### Mapa bivariado Fragmentação de habitat X BIOLOGICO ####
# Legenda
windows(5,5)
col.matrix.fragmentacao <- colmat(nquantiles=4, xlab="Fragmentação de habitat", ylab="Valor Biológico")
# Mapa bivariado. Ajustar o tamanho adequado da janela.
bivmap.fragmentacao <-bivariate.map(mesh_size ,rank, colormatrix=col.matrix.fragmentacao, nquantiles=4)
windows(70,60)
plot(bivmap.fragmentacao ,frame.plot=F,axes=F,box=F,add=F,legend=F,col=as.vector(col.matrix.fragmentacao))
###Salvar pela janela do grafico: arquivo-> salvar como


#### TODAS AS AMEAÇAS JUNTAS ####
ameacas<-(prop_paisagem+colisao_mean+mesh_size)/(3)
# Legenda
windows(5,5)
col.matrix.ameacas <- colmat(nquantiles=4, xlab="Risco Biológico", ylab="Valor Biológico")
# Mapa bivariado ameacas juntas X BIOLOGICO
bivmap.ameacas <-bivariate.map(ameacas,rank, colormatrix=col.matrix.ameacas , nquantiles=4)
windows(70,60)
plot(bivmap.ameacas ,frame.plot=F,axes=F,box=F,add=F,legend=F,col=as.vector(col.matrix.ameacas ))
###Salvar pela janela do grafico: arquivo-> salvar como






