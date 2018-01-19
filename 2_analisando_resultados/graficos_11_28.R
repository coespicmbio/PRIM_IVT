### Análise de resultados PRIM Estrada valor biologico do zonation raster rank ###
### Graficos de proporção da distribuição dos alvos por categoria de ameaça nas 4 classes de sensibilidade (flora e fauna)###
### Importante que todos os arquivos a serem utilizados estejam na mesma pasta. Por exemplo,
###feature_list tem que estar na pasta que tem o raster do resultado do zonation, que tem as Ups do cerrado e as pastas de 
###alvos de conservação com todos os respectivos rasters dentro. 
###O modelo de pasta para este script está em C:/Users/84835001168/Desktop/cerrado_resultado_prim####

###Dividindo o raster em quatro classes de sensibilidade###

library(raster)
setwd("D:/PRIM_estrada/_resultado/caatinga")

# Antes de carregar o raster, fazer um cópia dele no ArcGIS para 64bits. mas não é obrigatorio
# Carregar o raster rank resultado do Zonation
rank <- raster("rank/prim_estrada_caatinga.tif")

# Separa em 4 classes menos preocupante ate extremamente sensivel###
# Classe 1: <0.25
c1 <- (rank<=0.25)*1
plot(c1)
# Classe 2
c2 <- ((rank>0.25)*(rank<=0.5))*2
plot(c2)
# Classe 3
c3 <- ((rank>0.5)*(rank<=0.75))*3
plot(c3)
# Classe 4
c4 <- ((rank>0.75))*4
plot(c4)

# Raster com as 4 classes
rc <- c1+c2+c3+c4
plot(c1+c2+c3+c4)

# Carrega o raster ou shapefile de ups (converte para raster). tem que ser o raster q foi usado no zonation. atencao as letras maiusculas e underlines
up <- raster("./up/up_caatinga.tif")
plot(up)

# Total de células do valores diferentes de NA
ncell_bioma <- length(na.exclude(getValues(rc)))

# Proporção de cada classe ocupando o bioma (o numero 2 no final da sentença e relativo às casas decimais)###
c1_prop <- round(cellStats(c1, stat='sum', na.rm=TRUE)/ncell_bioma,2)
c2_prop <- round(cellStats(c2, stat='sum', na.rm=TRUE)/ncell_bioma,2)
c3_prop <- round(cellStats(c3, stat='sum', na.rm=TRUE)/ncell_bioma,2)
c4_prop <- round(cellStats(c4, stat='sum', na.rm=TRUE)/ncell_bioma,2)

# Carrega o arquivo spp dos alvos de conservacao###
###spp é o caminho do endereço dos raters dos alvos, a partir de alvos de conservação
spp <- read.table("features_list.txt",header = F,sep=" ")

##escolhe a coluna que interessa da planilha#
spp <- spp[,6]
#transforma a tabela em vetor, deixa de ter coluna e linha#
spp <- as.vector(spp)
#exclui todos os caracteres antes de "alvos_conservacao", ou seja, deixa no vetor apenas o caminho que me interessa para encontrar o raster
spp <- gsub(spp,pattern = ".*alvos_conservacao/", replacement = "")
##Caso na spp list (que foi obtida a partir do features-list) tiver as linhas de ameças, podemos elimina-las antes do rodar o loop. 
###mas mesmo que a gente não delete, o loop vai dar erro nessa parte, mas as tabelas serão criadas apesar do erro


##criando tabelas a serem preenchidas no loop abaixo###
##toda vez que vc rodar o loop e por acaso tiver que refaze-lo, os dados vem para a tabela, entao, SEMPRE CRIE as tabelas vazias (rode os comandos abaixo) antes de rodar o loop para evitar sobreposição de informação
tab_up <- data.frame("alvo"=character(0),"up"=numeric(0))
tab_cl <- data.frame("alvo"=character(0),"classe"=numeric(0))
tab_prop <- data.frame("alvo"=character(0),"classe_1"=numeric(0),"classe_2"=numeric(0),"classe_3"=numeric(0),"classe_4"=numeric(0))


#para construir o looping, preciso extrair o raster de cada alvo e para isso busco a lista na tabela com o endereço dos alvos. a partir daí os comandos pedem para eliminar tudo que 
##nao seja o nome da sp##

##Se o loop não funcionar, da para descobrir o erro individualmente por especie, começando em a<-spp[1], pula a linha seguinte (q começa com for...)
### e segue direto para o nome no alvo ( nome <- basename(a)). O [1] se refere à linha na planilha do alvo. Se o alvo está na linha 203, [203]###
#a <- spp[1]

for (a in spp){
  
  # Nome do alvo
  nome <- basename(a)
  nome <- gsub(pattern = "\\.tif$", replacement = "",nome)
  nome <- gsub(pattern = "caatinga_", replacement = "",nome)
  
  cat(nome,"\n")
  
  # Carrega o raster do alvo
  ra <- raster(a)
  
  # Transforma as celulas do raster que antes apresentavam valores continuos para binário
  ra_b <- ra>0
  
  # Numero de celulas emc ada classe
  n_c1 <- cellStats(c1*ra_b, stat='sum', na.rm=TRUE)
  n_c2 <- (cellStats(c2*ra_b, stat='sum', na.rm=TRUE))/2
  n_c3 <- (cellStats(c3*ra_b, stat='sum', na.rm=TRUE))/3
  n_c4 <- (cellStats(c4*ra_b, stat='sum', na.rm=TRUE))/4
  n_total <- n_c1 + n_c2 + n_c3 + n_c4
  
  # Proporcoes
  prop_c1 <- n_c1/n_total
  prop_c2 <- n_c2/n_total
  prop_c3 <- n_c3/n_total
  prop_c4 <- n_c4/n_total
  
  # Construindo tabela de proporcao
  tab_prop_a <- data.frame("alvo"=nome,"classe_1"=prop_c1,"classe_2"=prop_c2,"classe_3"=prop_c3,"classe_4"=prop_c4)
  
  
  # Classes e UPs em que o alvo tem ocorrência
  if (maxValue(ra_b)!=0){
    ra_b_pto <- rasterToPoints(ra_b,spatial = T,fun=function(x){x==1})
    a_cl <- sort(as.vector(na.exclude(unique(extract(rc,ra_b_pto,fun=max)))))
    a_up <- sort(as.vector(na.exclude(unique(extract(up,ra_b_pto,fun=max)))))
    
    tab_a_up <- as.data.frame(cbind(rep(nome,length(a_up)),a_up))
    names(tab_a_up) <- c("alvo","up")
    tab_a_cl <- as.data.frame(cbind(rep(nome,length(a_cl)),a_cl))
    names(tab_a_cl) <- c("alvo","classe")
    
    tab_up <- rbind(tab_up,tab_a_up)
    tab_cl <- rbind(tab_cl,tab_a_cl)}
  
  tab_prop <- rbind(tab_prop, tab_prop_a)
}


# Carrega a tabela com as categorias de ameaças
##Essa planilha eu fiz com base no arquivo "planilha_prim_estradas_ferrovias_MP1" que é um arquivo com varias abas. Peguei todas as sp
###de fauna ameaçadas e de flora e salvei. Na aba das sp de flora, não tem coluna de categoria, e sim de status (com os pesos relativos aos
### categorias de ameaça. Substituí peso 1 por Cr, 0.75 por En e 0.5 por VU###
cat <- read.table("especies_categoria.txt",sep="\t",header = T)

# merge
tab_up <- merge(tab_up,cat, by.x="alvo",by.y="alvo", all.x=T)
tab_cl <- merge(tab_cl,cat, by.x="alvo",by.y="alvo", all.x=T)
tab_prop <- merge(tab_prop,cat, by.x="alvo",by.y="alvo", all.x=T)

# Salva tabelas
write.table(tab_cl,"tabela_classes.txt",sep="\t",quote = F,row.names = F)
write.table(tab_up,"tabela_ups.txt",sep="\t",quote = F,row.names = F)
write.table(tab_prop,"tabela_proporcao_distribuicao.txt",sep="\t",quote = F,row.names = F)


###### ATENÇÃO ########################################################################################################################################################################################################################################################################
# # Abrir a planilha "tabela_proporcao_distribuicao.txt" no excel e inserir uma coluna "grupos" com "fauna", "flora", "fitofisionomias" e "cavernas". 
######################################################################################################################################################################################################################################################################################

# Carregar a planilha "tabela_proporcao_distribuicao.txt"
tab_prop <- read.table("tabela_proporcao_distribuicao.txt",sep="\t", header=T)

#### Tabela medias e sd ####
tab_med_sd <- data.frame("grupo" = character(0), "classe" = numeric(0),"categoria" = character(0), "media"=numeric(0),"sd"=numeric(0))

#### Fauna ####
fauna <- subset(tab_prop,grupo=="fauna")

# 
for (c in as.vector(unique(fauna$categoria))){
  
  #
  fa_cat <- subset(fauna,categoria==c)
  
  #
  tab <- as.data.frame(cbind(rep("fauna",4),classes <- 1:4,rep(c,4),sapply(fa_cat[,2:5],mean),sapply(fa_cat[,2:5],sd)),row.names = NULL)
  names(tab) <- c("grupo","classe","categoria","media","sd")
  
  #
  tab_med_sd <- rbind(tab_med_sd,tab)
}

#### Flora ####
flora <- subset(tab_prop,grupo=="flora")

# 
for (c in as.vector(unique(flora$categoria))){
  
  #
  flor_cat <- subset(flora,categoria==c)
  
  #
  tab <- as.data.frame(cbind(rep("flora",4),classes <- 1:4,rep(c,4),sapply(flor_cat[,2:5],function(x) mean(x, na.rm=TRUE)),sapply(flor_cat[,2:5],function(x) sd(x, na.rm=TRUE))),row.names = NULL)
  names(tab) <- c("grupo","classe","categoria","media","sd")
  
  #
  tab_med_sd <- rbind(tab_med_sd,tab)
}


#### Fitofisionomia ####
fito <- subset(tab_prop,grupo=="fitofisionomias")

#
tab <- as.data.frame(cbind(rep("fitofisionomias",4),classes <- 1:4,rep("NA",4),sapply(fito[,2:5],function(x) mean(x, na.rm=TRUE)),sapply(fito[,2:5],function(x) sd(x, na.rm=TRUE))),row.names = NULL)
names(tab) <- c("grupo","classe","categoria","media","sd")

#
tab_med_sd <- rbind(tab_med_sd,tab)


#### Cavernas ####
cave <- subset(tab_prop,grupo=="cavernas")

#
tab <- as.data.frame(cbind(rep("cavernas",4),classes <- 1:4,rep("NA",4),sapply(cave[,2:5],mean),sapply(cave[,2:5],sd)),row.names = NULL)
names(tab) <- c("grupo","classe","categoria","media","sd")

#
tab_med_sd <- rbind(tab_med_sd,tab)

# Substituindo os valores das classes que variam de 1 a 4, pelas categorias###
tab_med_sd$classe<-gsub(tab_med_sd$classe,pattern = "1",replacement = "Áreas Menos Preocupantes")
tab_med_sd$classe<-gsub(tab_med_sd$classe,pattern = "2",replacement = "Áreas Sensíveis")
tab_med_sd$classe<-gsub(tab_med_sd$classe,pattern = "3",replacement = "Áreas Muito Sensíveis")
tab_med_sd$classe<-gsub(tab_med_sd$classe,pattern = "4",replacement = "Áreas Extremamente Sensíveis")

# Colocando a ordem em que as classes aparecerão no gráfico, do mais sensível para o menos preocupante###
tab_med_sd$classe = factor(tab_med_sd$classe, levels=c("Áreas Extremamente Sensíveis",'Áreas Muito Sensíveis','Áreas Sensíveis','Áreas Menos Preocupantes'))
tab_med_sd$categoria = factor(tab_med_sd$categoria, levels=c("CR",'EN','VU',"NT","LC"))

# Salva a tabela
write.table(tab_med_sd,"tabela_proporcao_distribuicao_MEAN.txt",sep="\t",quote = F,row.names = F)
#####


#### GRÁFICOS ####
library(ggplot2)
#library(gridExtra)
###### ATENÇÃO ##################################################################################################################
### A partir de agora va para o excel e verticalize a planilha tabela_proporcao_distribuicao_MEAN.txt. Ao inves das classes
### estarem dispostas em quatro colunas diferentes, elas devem estar numa unica coluna chamada "classe"
#################################################################################################################################

#### Grafico proporcao da distribuicao ####

### Fauna e Flora ####

# Selecionando apenas linhas de fauna e flora
fauflor <- tab_med_sd[tab_med_sd$grupo =="fauna" | tab_med_sd$grupo=="flora",]

# Converte o tipo da coluna "media" e "sd" para numérico
fauflor$media<- as.numeric(as.character(fauflor$media))
fauflor$sd<- as.numeric(as.character(fauflor$sd))

# Separa as classes
fauflor_4 <- fauflor[fauflor$classe =="Áreas Extremamente Sensíveis",]
fauflor_3 <- fauflor[fauflor$classe =="Áreas Muito Sensíveis",]
fauflor_2 <- fauflor[fauflor$classe =="Áreas Sensíveis",]
fauflor_1 <- fauflor[fauflor$classe =="Áreas Menos Preocupantes",]

# Faz o gráfico de cada classe
# Espaço entre os gráficos de fauna e flora
pd<- position_dodge(.5) 

# Classe 4
g4 <- ggplot(fauflor_4, aes(x= categoria, y= media, color = grupo)) +
  geom_point(shape = 16,size  = 2,position = pd) +
  geom_errorbar(aes(ymin  = media - sd, ymax  = media + sd),width = 0.2,size  = 0.7,position = pd) +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme_bw() +
  theme(strip.text = element_text(colour = "white", face="bold"), strip.background = element_rect(colour = "black", fill = "#a20000"), legend.position="none",axis.title.y=element_text(size=12,face="bold", vjust = -0.5)) + 
  scale_colour_manual(values = c("saddlebrown", "darkgreen"))+
  xlab("") +
  ylab("Representatividade da Distribuição dos Alvos de Conservação")+
  facet_grid(. ~ classe)

# Classe 3
g3 <- ggplot(fauflor_3, aes(x= categoria, y= media, color = grupo)) +
  geom_point(shape = 16,size  = 2,position = pd) +
  geom_errorbar(aes(ymin  = media - sd, ymax  = media + sd),width = 0.2,size  = 0.7,position = pd) +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0,0.2,0.4,0.6,0.8,1),labels = NULL)+
  theme_bw() +
  theme(strip.text = element_text(colour = "black", face="bold"), strip.background = element_rect(colour = "black", fill = "#ff7c10"), legend.position="none",axis.title.x=element_text(size=12,face="bold", hjust = 1.5),plot.title = element_text(hjust = 0.1)) + 
  scale_colour_manual(values = c("saddlebrown", "darkgreen"))+
  ggtitle("Proporção da Distribuição dos Alvos de Conservação na Caatinga")+
  xlab("Categorias da IUCN") +
  ylab("")+
  facet_grid(. ~ classe)

# Classe 2
g2 <- ggplot(fauflor_2, aes(x= categoria, y= media, color = grupo)) +
  geom_point(shape = 16,size  = 2,position = pd) +
  geom_errorbar(aes(ymin  = media - sd, ymax  = media + sd),width = 0.2,size  = 0.7,position = pd) +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0,0.2,0.4,0.6,0.8,1),labels = NULL)+
  theme_bw() +
  theme(strip.text = element_text(colour = "black", face="bold"), strip.background = element_rect(colour = "black", fill = "#ffe200"), legend.position="none",plot.background = element_rect(fill = "transparent")) + 
  scale_colour_manual(values = c("saddlebrown", "darkgreen"))+
  xlab("") +
  ylab("")+
  facet_grid(. ~ classe)

# Classe 1
g1 <- ggplot(fauflor_1, aes(x= categoria, y= media, color = grupo)) +
  geom_point(shape = 16,size  = 2,position = pd) +
  geom_errorbar(aes(ymin  = media - sd, ymax  = media + sd),width = 0.2,size  = 0.7,position = pd) +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0,0.2,0.4,0.6,0.8,1), labels = NULL)+
  theme_bw() +
  theme(strip.text = element_text(colour = "white", face="bold"), strip.background = element_rect(colour = "black", fill = "#2a9600"),plot.background = element_rect(fill = "transparent")) + 
  scale_colour_manual(values = c("saddlebrown", "darkgreen"))+
  xlab("") +
  ylab("")+
  facet_grid(. ~ classe)

# Montagem dos Gráficos em um único grid ####
library(cowplot)

# Plota os gráficos em um grid
prow <- plot_grid( g4 + theme(legend.position="none"),
                   g3 + theme(legend.position="none"),
                   g2 + theme(legend.position="none"),
                   g1 + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1)

# Captura a legenda de um dos gráficos
legend <- get_legend(g1)

# Abre a Janela onde será feito o plot com as dimensões desejadas
windows(60,20)

# Plota os gráficos junto com a legenda na janela
plot_grid( prow, legend, rel_widths = c(3, .3))

########################################################################
#### ATENÇÃO: Salvar o gráfico pelo menu arquivo da própria  janela ####
########################################################################

### Cavernas e Fitofisionomias ####

# Selecionando apenas linhas de fauna e flora
cavfito <- tab_med_sd[tab_med_sd$grupo =="cavernas" | tab_med_sd$grupo=="fitofisionomias",]

# Converte o tipo da coluna "media" e "sd" para numérico
cavfito$media<- as.numeric(as.character(cavfito$media))
cavfito$sd<- as.numeric(as.character(cavfito$sd))

# Define que a ordem (da mais sensivel para a menos preocupante) seja refletida no gráfico. Caso contrário, será ordem alfabética.
cavfito$classe = factor(cavfito$classe, levels=c("Áreas Extremamente Sensíveis",'Áreas Muito Sensíveis','Áreas Sensíveis','Áreas Menos Preocupantes'))

# Como posicionar as barras no grafico###
pd<- position_dodge(.4) 

# Gráfico para as 4 classes
gcf <- ggplot(cavfito, aes(x= classe, y= media, color = grupo)) +
  geom_point(shape = 16,size  = 2,position = pd) +
  geom_errorbar(aes(ymin  = media - sd, ymax  = media + sd),width = 0.2,size  = 0.7,position = pd) +
  theme_bw() +
  ggtitle("Proporção da Distribuição dos Alvos de Conservação na Caatinga") +
  theme(plot.title = element_text(hjust = 0.5), strip.background = element_rect(colour = "black", fill = "gray")) + 
  scale_colour_manual(values = c("saddlebrown", "darkgreen"))+
  xlab("Sensibilidade das Áreas") +
  ylab("Representatividade da Distribuição dos Alvos de Conservação")


# Abre a Janela onde será feito o plot com as dimensões desejadas
windows(60,50)

# Plota os gráficos junto com a legenda na janela
gcf

########################################################################
#### ATENÇÃO: Salvar o gráfico pelo menu arquivo da própria  janela ####
########################################################################

