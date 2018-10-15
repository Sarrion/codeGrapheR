
stringSplitter <- function(string){
  for(i in 1:nchar(string))
    if(substr(string, i, i) == "(")
      break
  fun = paste0(substr(string, 1, i), ")")
  string = substr(string, i + 1, nchar(string))
  trigger = 1
  for(i in 1:nchar(string)){
    if(substr(string, i, i) == "(") 
      trigger = trigger + 1
    else if(substr(string, i, i) == ")")
      trigger = trigger - 1
    if(trigger == 0)
      break
  }
  result = list(fun = fun,
                insideFun = substr(string, 1, i - 1),
                remaining = substr(string, i + 1, nchar(string)))
  return(result)
}

roundFirst <- function(string){
  splitation = stringSplitter(string = string)
  data.frame(name = splitation$`fun`,
             layer = l,
             element = e,
             pointer = p[length(p)], 
             identifier = idAdder(name),
             cluster = clAdder(clZone)) ->> graph_data[nrow(graph_data) + 1, ]
  if(splitation$insideFun != ""){
    l <<- l + 1
    p <<- c(p, e[length(e)])
    e <<- c(e, 1)
    splitation$insideFun %>% 
      gsub(" ", "", .) %>% 
      strsplit(., split = ",") -> elements
    for(i in elements){
      deconstructor(i)
      e[length(e)] <<- e[length(e)] + 1
    }
    p <<- p[-length(p)]
    e <<- e[-length(e)]
  }
  if(splitation$remaining != "")
    deconstructor(splitation$remaining)
  l <<- l - 1
}



