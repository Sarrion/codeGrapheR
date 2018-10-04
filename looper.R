numOpPar <- ifelse(test = sapply(gregexpr("\\{", string), function(x) x[1]) < 0, yes = 0, no = sapply(gregexpr("\\{", string), length))
numClPar <- ifelse(test = sapply(gregexpr("\\}", string), function(x) x[1]) < 0, yes = 0, no = sapply(gregexpr("\\}", string), length))
totalPar <- numOpPar - numClPar

cumPar = 0
indxForRmv <- c()
for(i in length(string):2){
  cumPar <- cumPar + totalPar[i]
  if(cumPar < 0){
    string[i - 1] <- paste0(string[i - 1], ";", string[i])
    indxForRmv <- c(indxForRmv, i)
  }
}

loopString <- grep("\\{", string, value = T)
string <- string[-indxForRmv]
string <- string[!grepl("\\{", string)]