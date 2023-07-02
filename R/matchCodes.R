#
#
# This is the function matchCodes that counts
# the cooccurrences of terms from a dictionary 
# in a corpus. 
#
#' @import reshape2
#' @import stringi
#' @import quanteda
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @export
matchCodes <- function(corpus, dic, level=1, remove.self=TRUE, quietly=FALSE){
  
  fc <- quanteda::fcm(quanteda::tokens(corpus))
  ff <- quanteda::fcm_select(fc, pattern = unlist(dic), valuetype = "regex")
  
  dm <- quanteda::convert(ff, "data.frame")
  dm <- reshape2::melt(dm, id.vars = "doc_id")
  dm <- dm[dm$value>0,]
  dm$term1 <- NA
  dm$term2 <- NA
  
  if(level>1){
    dic <- unlist(dic, recursive = F)
    dic <- quanteda::dictionary(dic)
    nm <- names(dic)
    nma <- stringi::stri_split_fixed(nm, ".", 2, simplify = T)[,2]
  }else{
    nm <- names(dic)
    nma <- nm  
  }
  
  if(quietly==FALSE){
    pb <- utils::txtProgressBar(min = 0, max=length(nm), style=3, char="=", width = 50)
  }
  
  for(i in 1:length(nm)){
    
    ky <- unlist(dic[nm[i]], use.names = F)
    
    for(k in 1:length(ky)){
      
      dm$term1[which(stringi::stri_detect_regex(dm$doc_id, pattern = ky[k], case_insensitive=T)==TRUE)] <- nma[i]
      
      dm$term2[which(stringi::stri_detect_regex(dm$variable, pattern = ky[k], case_insensitive=T)==TRUE)] <- nma[i]
      
      
    }  
    
    if(quietly==FALSE){
      utils::setTxtProgressBar(pb, value = i)
    }
  }
  
  dm <- aggregate(list(value=dm$value), by=list(term1=dm$term1, term2=dm$term2), sum, na.rm=T)
  
  dm <- aggregate(value~term1 + term2, data=transform(dm, term1 = pmin(dm$term1, dm$term2),term2 = pmax(dm$term1, dm$term2)), sum, na.rm = TRUE)
  
  dm <- dm[order(dm$term1, dm$term2),]
  
  if(remove.self==TRUE){
    dm <- dm[dm$term1!=dm$term2,]
  }
  
  return(dm)
}  
