string <-'ruta<-"/Advanced_Analytics/Data_Science_Des/PROYECTO_WAVE/"

#Libreria
library(xlsx)
library(data.table)

#Leemos el diccionario
diccionario<-read.xlsx(paste0(ruta,"02_GENERACION_TABLONES/Resultados/diccionario.xlsx"),
sheetIndex = 1)

diccionario<-diccionario[!is.na(diccionario$Characteristic),]
#Nos quedamos con las variables de interes
diccionario<-diccionario[diccionario$ML.Model.A%in%"Y",]

#Leemos los datos
datos<-read.csv(paste0(ruta,"02_GENERACION_TABLONES/Resultados/EQX_BLEND_RETRO_iteration7_04092018.csv"),
stringsAsFactors = FALSE,skipNul = TRUE)

#Nos quedamos con los registros para el segmento LC
datos<-datos[datos$LEG_ENT_TYPE=="LC",]
datos<-datos[datos$perf_excl==0,]

#Seleccionamos las variables de la tabla que han de estar y aquellas que segun el diccionario tambien 

#variables que han de estar
variables_ob<-c("uid","CRN","CKEY1","ckey_1","ckey_2","ckey_3","nckey","sic_code","LEG_ENT_TYPE","DEV_WGT2","DEV_BAD2","NEW_POP_FLAG") 

#Creamos 3 vectores donde se incopore el _1, _2, y _3 por rapidez lo hacemos asi
variables_diccionario<-as.character(diccionario$Characteristic)

variables_diccionario_1<-variables_diccionario
variables_diccionario_1<-paste0(variables_diccionario_1,"_1")

variables_diccionario_2<-variables_diccionario
variables_diccionario_2<-paste0(variables_diccionario_2,"_2")

variables_diccionario_3<-variables_diccionario
variables_diccionario_3<-paste0(variables_diccionario_3,"_3")

#Unimos todas las variables
variables_diccionario<-c(variables_diccionario,variables_diccionario_1,variables_diccionario_2,variables_diccionario_3)

#Cuales estan en el dataset
variables_diccionario_comunes<-names(datos)[names(datos)%in%variables_diccionario]

#Nos quedamos con las variables que queremos y con las variables que estan en diccionario
variables_finales<-unique(c(variables_ob,variables_diccionario_comunes))

#Nos quedamos con el dataset acotado a estas variables
datos<-datos[c(variables_finales)]

#Buscamos los valores que hay que imputar a missing solo sobre las variables que no esten en variables_ob
variables_modificar<-names(datos)[!names(datos)%in%variables_ob]

#Cualquier valor con C, T, etc sera un NA
lista_resumen<-list()
for (z in 1:length(variables_modificar)){
print(z/length(variables_modificar)*100)
var<-variables_modificar[z]
#Para cada valor que coincida con un numero como los del vectos siguiente, se le imputara un NA
datos[,var][datos[,var]%in%c("#","_","","T","M","C","H","F","G","K","P","E","I")]<-NA
# datos[,var][datos[,var]%in%c("_","","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","x","Y","Z")]<-NA
}


#Hay que buscar las variables que son categoricas para darle categoria Missing a los valores NA, de cara a la
#Creacion de dummys.

#Es necesario evaluar todas las variables

#Variables que son todo NA
variables_na<-names(datos)[sapply(datos,function(x) all(is.na(x)))]

#Nos quedamos con el resto de variables
datos<-datos[c(names(datos)[!names(datos)%in%variables_na])]

#Otras variables a eliminar
variables_quitar<-names(datos)[names(datos)%in%c("B009_GEN","B010_GEN","D020_01_GEN","D020_02_GEN","D020_03_GEN","D020_04_GEN","J128_01_GEN","J128_02_GEN","J711_01_GEN","J711_02_GEN","J711_03_GEN","INLTDFDF","INLTDFST","INSTDTAG","LatestSatisfiedDate","Latestcurrccjdate","Latestsatisccjsdate","Previousnamelatestchangedate","YearEndDate","X_BUSINESS_TYPE","NEW_POP_FLAG")]
datos<-datos[c(names(datos)[!names(datos)%in%variables_quitar])]
variables_quitar<-names(datos)[names(datos)%in%c("CRN","CKEY1","ckey_1","ckey_2","ckey_3","LEG_ENT_TYPE","app_date","ckey")]
datos<-datos[c(names(datos)[!names(datos)%in%variables_quitar])]

#Realizamos conteos sobre estas variables
lista_resumen<-list()
for (z in 1:length(names(datos))){
print(z/length(names(datos))*100)

var<-names(datos)[z]
tables<-as.data.frame(t(table(datos[var],useNA = "always")))
tables<-tables[c("Var2" ,"Freq")]
names(tables)<-c("valor","conteo")
tables$variable<-var
tables$clase_actual<-class(datos[,var])

if(length(as.numeric(datos[,var])[is.na(as.numeric(datos[,var]))])==nrow(datos)){
tables$numeros<-"No tiene" 
}else{
tables$numeros<-"Si tiene" 
}
tables<-tables[c("variable","valor","conteo","numeros")]
tables$valor<-as.character(tables$valor)

tables<-tables[tables$conteo>=1,]

tables$valores_distintos<-length(unique(tables$valor))
tables$ratio<-tables$conteo/nrow(datos)*100
tables<-tables[order(tables$conteo,decreasing = TRUE),]
lista_resumen[[z]]<-tables
rm(tables)
}

Agrupacion_variables<-as.data.frame(rbindlist(lista_resumen))'


string <- unlist(strsplit(string, "\n"))
# string <- grep("(<-|\\(|\\))", string, value = T)


#----Aligner stage----
numOpPar <- ifelse(test = sapply(gregexpr("\\(", string), function(x) x[1]) < 0, yes = 0, no = sapply(gregexpr("\\(", string), length))
numClPar <- ifelse(test = sapply(gregexpr("\\)", string), function(x) x[1]) < 0, yes = 0, no = sapply(gregexpr("\\)", string), length))
totalPar <- numOpPar - numClPar
cumPar = 0
indxForRmv <- c()
for(i in length(string):2){
  cumPar <- cumPar + totalPar[i]
  print(cumPar)
  if(cumPar < 0){
    string[i - 1] <- paste0(string[i - 1], string[i])
    indxForRmv <- c(indxForRmv, i)
  }
}
string <- string[-indxForRmv]

# Cleaning non-useful lines: removing comments, void lines and {} symbols
string <- gsub("\\{|\\}", "", string)
string <- string[!(grepl(pattern = "#", string) | string == "")]

# string <- gsub(pattern = "<-", replacement = ",", x = string)
masterString <- string

