##########################################################
#### Decisao de qual UC sera representada pela celula ####
#### quando UCs estao presentes na mesma celula ##########
##########################################################

#library(plyr)
setwd("D:/PRIM_estrada/ups")

# Ler tabela de entrada
tab <- read.table("Amazonia.txt",header=T,sep=",")

## Converte os caracteres da tabela em numeros
# PIUS
PIUS <- as.vector(tab$SiglaGrupo)
PIUS <- gsub(pattern = "PI",replacement = 2, x=PIUS)
PIUS <- gsub(pattern = "US",replacement = 1, x=PIUS)
PIUS <- as.numeric(PIUS)
unique(PIUS) # Confere se todos os valores foram transformados

# ESFERA
ESFERA <- as.vector(tab$administra)
ESFERA <- gsub(pattern = "Federal",replacement = 3, x=ESFERA)
ESFERA <- gsub(pattern = "estadual",replacement = 2, x=ESFERA)
ESFERA <- gsub(pattern = "municipal",replacement = 1, x=ESFERA)
ESFERA <- as.numeric(ESFERA)
unique(ESFERA)# Confere se todos os valores foram transformados

# CATEGORIA
CATEGORIA <- as.vector(tab$Cateoria)
CATEGORIA <- gsub(pattern = "REBIO",replacement = 9, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "ESEC",replacement = 8, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "PARQUE",replacement = 7, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "REVIS",replacement = 6, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "MN",replacement = 5, x=CATEGORIA)
#CATEGORIA <- gsub(pattern = "UCPI",replacement = 6, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "RPPN",replacement = 4, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "RESEX",replacement = 3, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "RDS",replacement = 3, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "FLORESTA",replacement = 3, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "ARIE",replacement = 2, x=CATEGORIA)
CATEGORIA <- gsub(pattern = "APA",replacement = 1, x=CATEGORIA)
#CATEGORIA <- gsub(pattern = "ND",replacement = 1, x=CATEGORIA)
CATEGORIA <- as.numeric(CATEGORIA)
unique(CATEGORIA) # Confere se todos os valores foram transformados

# AREA
AREA <- as.numeric(tab$Proporcao)

# Ano Criacao
ANO <- as.numeric(tab$anoCriacao)

# Tabela numerica
tab_num <- cbind(tab[,11],tab[,2:3],PIUS,ESFERA,CATEGORIA,ANO,AREA,tab[,9])
names(tab_num)<-c("Id","codigoCnuc","nome","PIUS","ESFERA","CATEGORIA","ANO","AREA","UP_UC")
#tab_num <- subset(tab_num,COD_UC!="") # retira as linhas que não são UC

# Tabela de UCs
UCs <-as.data.frame(unique(tab_num$codigoCnuc))
names(UCs) <- "codigoCnuc"
#tab_ucs <- tab_num
#tab_ucs <- tab_ucs[!duplicated(tab_ucs[,1]),]


# Tabela de frequencia para uc. Conta em quantas celulas a UC esta contida.
tab_freq <- as.data.frame(table(tab_num$codigoCnuc))
names(tab_freq)<- c("codigoCnuc","Frequencia")
#tab_freq <- tab_freq[-1,]

# UCs que aparecem em apenas uma celula
#UC_1 <- subset(tab_freq, Frequencia == 1)

# Merge da tab_num com a Frequencia das UCs
tab_num <- merge(tab_num,tab_freq, by="codigoCnuc", all = TRUE)

# ID das celulas
id_cel <- unique(tab_num$Id)

# Cria uma tabela para receber as linhas escolhidas
tab_final <- data.frame(row.names=NULL)

# Cria uma tabela para receber casos de ID repetidos
#tab_repetidos <- data.frame(row.names=NULL)

# For loop para cada valor de ID_CEL
# Valor para ver o progresso
p <- 1

for (i in id_cel) {
  
  # Separa as linhas da tabela de um celula
  tab_id <- data.frame(subset(tab_num,Id==i))
  
  # Imprimi no console o progresso
  cat("Id:", i,"Progresso:", round(((p/length(id_cel))*100),2) ,"%","\n")
  

    # Prioriza as UCs q estao somente em uma celula
    if (1 %in% tab_id$Frequencia == TRUE){
      tab_id_1 <- data.frame(subset(tab_id,Frequencia == 1))
    } else { tab_id_1 <- tab_id }
    
    # Separa as linhas que tem o maior valor de ESFERA seguindo a ordem decrescente federal>estadual>municipal
    tab_esfera <- data.frame(subset(tab_id_1,ESFERA == max(tab_id_1$ESFERA)))
    
    # Separa as linhas que tem o maior valor de CATEGORIA seguindo a ordem decrescente 
    # 8)REBIO    7) ESEC    6) PARNA   5)REVIS    4)MONA    3)RPPN  2)RESEX/RDS/FLONA   1) ARIE 0)APA
    tab_categoria <- data.frame(subset(tab_esfera,CATEGORIA == max(tab_esfera$CATEGORIA)))
    
    # Separa a linha que tiver maior area
    tab_area <- data.frame(subset(tab_categoria,AREA == max(tab_categoria$AREA)))
    
    # Separa a linha que tiver menor ano
    tab_ano <- data.frame(subset(tab_area,ANO == min(tab_area$ANO)))
    
    # Adiciona a linha escolhida na tabela final
    tab_final <- rbind(tab_final,tab_ano)  
    
    # Progresso
    p <- p + 1
  
} # Fecha For Loop


# Escreve a tabela final
write.table(tab_final,file="tab_UP_amz_.txt",quote=FALSE,sep = "\t",row.names = FALSE)

# Desliga o PC
#system('shutdown -s')
# Ler a tabela final
#tab_final <- read.csv("tab_final_2.txt",header=T,sep="\t")


#### Ucs faltantes ####
ucs_final <- as.data.frame(unique(tab_final$codigoCnuc))
ucs_final$presenca <- 1
names(ucs_final) <- c("codigoCnuc","presenca")
uc_faltantes <- merge(x=UCs,y =ucs_final,by="codigoCnuc",all= T)
uc_faltantes1 <- uc_faltantes[is.na(uc_faltantes$presenca),]

write.table(uc_faltantes1,file="tab_UP_amz_FALTANTES.txt",quote=FALSE,sep = "\t",row.names = FALSE)


##############################

problema <- data.frame(subset(tab_num,codigoCnuc=="0000.53.1658"))

problema <- tab_num[tab_num$codigoCnuc %in% uc_faltantes1$codigoCnuc, ]


uc_faltantes <- merge(x=tab_ucs,y =ucs_final,by="COD_UC",all.x=TRUE)
uc_faltantes1 <- subset(uc_faltantes, is.na(PRESENTE))
uc_faltantes1 <- merge(x=uc_faltantes1, y=tab_ucs, by="COD_UC",all.x=TRUE)
uc_faltantes_Fed <- subset(uc_faltantes1, ESFERA==3)
uc_faltantes_Est <- subset(uc_faltantes1, ESFERA==2)
uc_faltantes_Mun <- subset(uc_faltantes1, ESFERA==1)
uc_faltantes_Plan <- subset(uc_faltantes1, TIPO ==1)

write.table(uc_taltantes_Plan,file="tab_faltantes_Planejadas.txt",quote=FALSE,sep = "\t",row.names = FALSE)

#### Checar id_cel repetidos ####
# Ler a tabela final
tab_final <- read.csv("tab_final_1.txt",header=T,sep="\t")
# Checa quais celulas estao repetidas
repetidos <- as.character(duplicated(tab_final$ID_CEL))
"TRUE" %in% repetidos
tab_final_repetidos <- cbind(tab_final,repetidos)
tab_final_repetidos <- subset(tab_final_repetidos, repetidos=="TRUE")
tab_final_repetidos$ID_CEL

# Cria uma tabela com as celulas repetidas
tab_repetidos_all <- merge(tab_final,tab_final_repetidos, by="ID_CEL",all=FALSE)
#### ####



#### Checar se todas as UCs estão presentes ####

# Lista dos codigos CNUC
COD_UC <- unique(tab_num$COD_UC)

# Cria uma tabela para receber a checagem
tab_final_faltantes <- data.frame(COD_UC= numeric(0),REPETE=character(0),row.names=NULL)  

# Roda a checagem
for (cod in COD_UC) {
  linha <- data.frame(cod,cod %in% tab_final$COD_UC)
  tab_final_faltantes <- rbind(tab_final_faltantes,linha)
  cat(cod,cod %in% tab_final$COD_UC, "\n")
}
names(tab_final_faltantes) <- c("CNUC","PRESENTE")

# Codigos que estao faltantes  
uc_faltantes <- subset(tab_final_faltantes, PRESENTE == "FALSE") 

#### ####
write.table(tab_final_repetidos,file="tab_final_repetidos.txt",quote=FALSE,sep = "\t",row.names = FALSE)



