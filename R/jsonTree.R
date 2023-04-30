#
#
# This is the function jsonTree.
###
### jsonTree
### 
### Funcion que prepara los datos como un json anidado de 2 niveles
###
# Parametros
# d - data.frame con al menos cuatro variable
#     grupo1 - grupo principal que anidara a los demas 
#     grupo2 - grupo secundario
#     elements - elemenos que compondran el grupo secundario
#     value - valor o frecuencia de los grupos secundarios
#     
#     ejemplo:  Personaje=Joao Grilo (grupo1), 
#               tipo_palabra=Verbo (grupo2),
#               Palabra=cantar (elements),
#               Freq=3 (value).
# groups - vetor de caracter con el nombre de las variables
#          correspondientes a los dos grupos
# elements - nombre de la variable con las unidades basicas
# value - nombre de la variable con el valor relativo a las unidades basicas
#
#' @export
jsonTree <- function(data, groups, elements, value){
  
  d <- data.frame(data)
  
  g1 <- unique(d[,groups[1]])
  g2 <- unique(d[,groups[2]])
  
  resB <- vector()
  
  for(i in 1:length(g1)){
    
    x <- d[d[[groups[1]]]==g1[i],]
    
    resA <- vector()
    
    for(k in 1:length(g2)){
      
      p <- x[x[[groups[2]]]==g2[k],]
      p <-p[! is.na(p[,groups[2]]),]
      
      if(nrow(p)>0){
        vals <- paste0("{'name': '", 
                       p[,elements],
                       "', 'value': ", 
                       p[,value],"}", 
                       collapse = ",\n")
        
        resA <- c(resA, paste0("\n{'name': '",
                               unique(p[,groups[2]]), 
                               "' , 'children': [",
                               vals, "]}"))
        
      }
    }
    
    resB <- c(resB, paste0(paste0("\n{'name': '",
                                  g1[i], 
                                  "' , 'children': ["), 
                           paste0(resA, collapse = ","), "]},"))
    
  }  
  
  resB <- paste(resB, collapse = " ")
  resB <- substr(resB, 1, nchar(resB)-1)
  
  return(resB)
  
}
