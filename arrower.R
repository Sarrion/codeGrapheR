#########################
####### FIRST STAGE #######
#########################
functionLines <- grepl(pattern = "\\(|\\)", string)

string <- gsub(pattern = "\\(.*\\)", replacement = "\\(\\)", string)

string <- gsub("\\[.*\\]", "", string)


# regmatches(string, gregexpr("^.*<-", string))
#data.frame
edges <- data.frame(
  from = substr(string, lapply(gregexpr("<-", string), function(x) x[[1]] + 2 ), nchar(string)) ,  
  to = substr(string, 1, lapply(gregexpr("<-", string), function(x) x[[1]] - 1 ))
)
masterEdges <- edges



############################
######## SECOND STAGE ########
############################

string <- masterString

string <- grep(pattern = "\\(|\\)", string, value = T)

string <- gsub(pattern = "^.*\\(", replacement = "", string)
string <- gsub(pattern = "\\).*$", replacement = "", string)

string <- gsub("\\[.*\\]", "", string)

masterEdges <- rbind(
  masterEdges, 
  data.frame(
    from = string,  
    to = edges[1][functionLines, ]
  )
)


