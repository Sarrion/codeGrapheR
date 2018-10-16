
squareSplitter <- function(string){
  for(i in 1:nchar(string))
    if(substr(string, i, i) == "[")
      break
  leftStr = substr(string, 1, i - 1)
  string = substr(string, i + 1, nchar(string))
  trigger = 1
  for(i in 1:nchar(string)){
    if(substr(string, i, i) == "[") 
      trigger = trigger + 1
    else if(substr(string, i, i) == "]")
      trigger = trigger - 1
    if(trigger == 0)
      break
  }
  return(list(leftStr = leftStr,
              inside = substr(string, 1, i - 1),
              remaining = substr(string, i + 1, nchar(string)))  )
}


squareFirst <- function(string){
  splitation = squareSplitter(string = string)
  data.frame(name = splitation$`leftStr`,
             layer = l,
             element = e,
             pointer = p[length(p)], 
             identifier = idAdder(name),
             cluster = clAdder(clZone)) ->> graph_data[nrow(graph_data) + 1, ]
  l <<- l + 1
  p <<- c(p, e[length(e)])
  e <<- c(e, 1)
  data.frame(name = "[]",
             layer = l,
             element = e,
             pointer = p[length(p)], 
             identifier = idAdder("[]"),
             cluster = clAdder(clZone)) ->> graph_data[nrow(graph_data) + 1, ]
  if(splitation$insideFun != ""){
    l <<- l + 1
    p <<- c(p, e[length(e)])
    e <<- c(e, 1)
    splitation$inside %>% 
      gsub(" ", "", .) %>% 
      strsplit(., split = ",") -> elements
    for(i in elements){
      deconstructor(i)
      e[length(e)] <<- e[length(e)] + 1
    }
    p <<- p[-length(p)]
    e <<- e[-length(e)]
  }
  p <<- p[-length(p)]
  e <<- e[-length(e)]
  l <<- l - 1
  if(splitation$remaining != "")
    deconstructor(splitation$remaining)
  l <<- l - 1
}


