library(stringr)

string %>%
  grep("<-", ., value = T) -> arrowLines

arrowLines %>% 
  gsub("<-.*$", "", .) %>%
  gsub(pattern = "\\(.*\\)", replacement = "\\(\\)", .) %>% 
  gsub(pattern = "\\[.*\\]", replacement = "", .) -> leftSide

arrowLines %>% 
  gsub("^.*<-", "", .) %>% 
  sapply(function(x){ 
    if( !grepl("\\(", x) && !grepl("\\[", x) )
      return(x)
    if(  grepl("\\(", x) && !grepl("\\[", x) )
      return(gsub("\\(.*\\)$", "\\(\\)", x))
    if( !grepl("\\(", x) &&  grepl("\\[", x) )
      return(gsub("\\[.*\\]$", "", x))
    if(  grepl("\\(", x) &&  grepl("\\[", x) ){
      if(str_locate(x, "\\(")[,1] < str_locate(x, "\\[")[,1])
        return(
          gsub("\\(.*\\)", "\\(\\)", gsub("\\[.*\\]", "", x))
        )
      else
        return(
          gsub("\\[.*\\]$", "", x)
        )
    }
  }
  ) %>% 
  as.character() -> rightSide
  
edges <- data.frame(
    from = rightSide,  
    to = leftSide
  )


