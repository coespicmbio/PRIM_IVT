

# Ajeitar o nome do arquivo 

flora <- list.files("D:/PRIM_estrada/_analise_priorizacao/cerrado/alvos_conservacao/especies_flora", pattern = "\\.tif$",all.files = T, full.names = T)


for (f in flora){
  
  nome <- gsub(pattern="cerrado_", replacement = "",f)
  nome <- gsub(pattern = " ", replacement = "_",nome)
  
  file.rename(from=f, to=nome)
}


habitat <- list.files("./cerrado/alvos_conservacao/habitats_especificos", pattern = "\\.tif$", full.names = T)

for (f in habitat){
  
  nome <- gsub(pattern="__", replacement = "",f)
  
  file.rename(from=f, to=nome)
}


