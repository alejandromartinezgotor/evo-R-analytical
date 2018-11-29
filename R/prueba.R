#Probar: poner mayor peso al múltiplicador "número de caracteres comunes respecto a b",
# siendo b el nombre o apellido real (normalmente más largo).

library("stringdist")
library(tidyr)
library(data.table)
library(plyr)
library(stringi)
library(XML)

# LECTURA DE FICHEROS.
root <- "/Users/alejandromartinez/Documents/EVOprojects/evo-R-analytical/"

reviews <- function(root){
  
params <- xmlToList(xmlParse(paste0(root,"config/config.xml")))
cat("Loading data...")

#setwd("/Users/alejandromartinez/Documents/googlePlay-reviews/")
reviews <- read.delim2(params$pathTable1, sep = ';', header = F, stringsAsFactors = F,
                       col.names = c("Name","Date"), 
                       colClasses = c("character","character"))

clients <- read.delim2(params$pathTable2, sep = ',', header = T, stringsAsFactors = F)

# PREPROCESAMIENTO

cat("Preprocessing...")

clients <- clients[!is.na(clients$DES_NOMBRE),]
clients <- clients[!is.na(clients$DES_PRIMER_APELLIDO),]
clients <- clients[!is.na(clients$DES_SEGUNDO_APELLIDO),]
clients$DES_PRIMER_APELLIDO <-
  replace(clients$DES_PRIMER_APELLIDO, which(clients$DES_PRIMER_APELLIDO == "APELLIDO 1"),"")
clients$COINCIDENCIAS <- rep(0,nrow(clients))
clients$DISTANCE <- 0

fullNameclients <- clients[c("DES_NOMBRE","DES_PRIMER_APELLIDO","DES_SEGUNDO_APELLIDO")]

fullNameclients <- unlist(unite(data = fullNameclients,"unificada",sep = " ",remove = TRUE),use.names = FALSE)
fullNameclients <- gsub("\\.", "", fullNameclients)
reviews <- reviews[reviews$Name != "",] 
nrowReviews <- nrow(reviews)
reviews <- stri_trans_general(reviews$Name,"Latin-ASCII")
reviewsUpper <- toupper(reviews)

# ALGORITMO : CALCULO DE DISTANCIAS(JARO - WIL)

cat("Executing algorithm")

l <- list()
for (index in 1:nrowReviews) {
  namesSplitted <- strsplit(x = reviewsUpper[index],split = " ")[[1]]
  numWords <- length(namesSplitted)
  
  if (numWords == 2){
    #prueba1 <- stringdist(reviewsUpper[index], clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1)
    
    prueba2 <- stringdist(namesSplitted[1],clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1) +
      stringdist(namesSplitted[2],clients$DES_PRIMER_APELLIDO, method = "jw",p = 0.1, bt = 0.1)
    
    prueba3 <- stringdist(namesSplitted[1],clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1) +
      stringdist(namesSplitted[2],clients$DES_SEGUNDO_APELLIDO,  method = "jw",p = 0.1, bt = 0.1)
    
    result <- which(min(min(prueba2), min(prueba3)) ==
                      c(min(prueba2), min(prueba3)))[1] + 1
    
    pruebaF <- eval(parse(text = paste0("prueba",result)))
    
    df<- clients[which(pruebaF == min(pruebaF)),]
    df$COINCIDENCIAS <- nrow(df)
    df$DISTANCE <- min(pruebaF)
    #eval(parse(text = paste0("l$",reviews[index], "<-", df)))
    l[[index]] <- df
  }
  
  else if(numWords == 3){
    prueba1 <- stringdist(namesSplitted[1],clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1) +
      stringdist(namesSplitted[2],clients$DES_PRIMER_APELLIDO, method = "jw",p = 0.1, bt = 0.1)  +
      stringdist(namesSplitted[3],clients$DES_SEGUNDO_APELLIDO,  method = "jw",p = 0.1, bt = 0.1)
    
    prueba2 <- stringdist(paste(namesSplitted[1],namesSplitted[2]),clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1) +
      stringdist(namesSplitted[3],clients$DES_PRIMER_APELLIDO, method = "jw",p = 0.1, bt = 0.1)
    
    prueba3 <- stringdist(paste(namesSplitted[1],namesSplitted[2]),clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1) +
      stringdist(namesSplitted[3],clients$DES_SEGUNDO_APELLIDO,  method = "jw",p = 0.1, bt = 0.1)
    
    result <- which(min(min(prueba1), min(prueba2), min(prueba3)) ==
                      c(min(prueba1), min(prueba2), min(prueba3)))[1]
    
    pruebaF <- eval(parse(text = paste0("prueba",result)))
    
    df<- clients[which(pruebaF == min(pruebaF)),]
    df$COINCIDENCIAS <- nrow(df)
    df$DISTANCE <- min(pruebaF)
    l[[index]] <- df
  }
  else if(numWords == 4){
    prueba1 <- stringdist(paste(namesSplitted[1],namesSplitted[2]),clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1) +
      stringdist(namesSplitted[3],clients$DES_PRIMER_APELLIDO, method = "jw",p = 0.1, bt = 0.1)  +
      stringdist(namesSplitted[4],clients$DES_SEGUNDO_APELLIDO,  method = "jw",p = 0.1, bt = 0.1)
    
    prueba2 <- stringdist(paste(namesSplitted[1],namesSplitted[2],namesSplitted[3]),clients$DES_NOMBRE, method = "jw",p = 0.1, bt = 0.1) +
      stringdist(namesSplitted[4],clients$DES_PRIMER_APELLIDO, method = "jw",p = 0.1, bt = 0.1)
    
    result <- which(min(min(prueba1), min(prueba2)) ==
                      c(min(prueba1), min(prueba2)))[1]
    
    pruebaF <- eval(parse(text = paste0("prueba",result)))
    
    df<- clients[which(pruebaF == min(pruebaF)),]
    df$COINCIDENCIAS <- nrow(df)
    df$DISTANCE <- min(pruebaF)
    l[[index]] <- df
  }
  else{
    result <- stringdist(reviewsUpper[index],fullNameclients, method = "jw",p = 0.1, bt = 0.1)
    df <- clients[which(result == min(result)),]
    df$COINCIDENCIAS <- nrow(df)
    df$DISTANCE <- min(result)
    #eval(parse(text = paste0("l$",reviews[index], "<-", df)))
    l[[index]] <- df
  }
}
names(l) <- reviews
finalDF <- rbindlist(l = l,idcol = "NICKNAME")

cat("Saving results...")

#ORDENAR DATA FRAME

finalDFOrdered <- finalDF[order(finalDF$COINCIDENCIAS,finalDF$DISTANCE),]

#SALVAR DATA FRAME ORDENADO

write.csv2(x = finalDFOrdered,
           file = paste0(root,"/output/",params$outputName,".csv"),
           row.names = TRUE)
}
