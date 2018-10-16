
idAdder <- function(string){
  if(string %in% idTable$name){
    idTable %>% 
      filter(name == string) %>% 
      select(appearences) %>%
      (function(x) x + 1) -> idTable$appearances[which(idTable$name) == string]
    return(idTable$appearances[which(idTable$name) == string])  
  }
  else
    return(0)
}

clAdder <- function(clZone){
  if(clZone)
    return(lastClZone)
  else
    return(0)
}


