

deconstructor <- function(string){
  if( !grepl("\\(", x) && !grepl("\\[", x) ){
    data.frame(name = string,
               layer = l,
               element = e,
               pointer = p[length(p)], 
               identifier = id,
               cluster = cl) ->> graph_data[nrow(graph_data) + 1, ]
    return(0)
  }
  if(  grepl("\\(", string) && !grepl("\\[", string) ){
    roundFirst(string)
    return(0)
  }
  if( !grepl("\\(", string) &&  grepl("\\[", string) ){
    squareFirst(string)
    return(0)
  }
  if(  grepl("\\(", string) &&  grepl("\\[", string) ){
    if(str_locate(x, "\\(")[,1] < str_locate(x, "\\[")[,1]){
      roundFirst(string)
      return(0)
    }
    else{
      squareFirst(string)
      return(0)
    }
  }
}