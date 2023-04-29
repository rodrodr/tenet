#
#
# Function wordtree
# Creates a wordtree for visualizing keywords in context
#' @import quanteda
#' @import stringi
#' @import tm
#' @export
wordtree <- function(
                    corpus, 
                    keyword, 
                    window=5, 
                    direction="double", 
                    rm.stop=FALSE, 
                    lang="es", 
                    url.return=FALSE, 
                    height=900){

  kw <- tolower(keyword)
  cs <- quanteda::corpus_reshape(corpus, to = "sentences")
  
  sample <- as.character(cs)[grep(kw, as.character(cs), ignore.case = T)]
  
  if(rm.stop==T){
    
    stp <- quanteda::stopwords(lang)
    
    sample <- tm::removeWords(sample, stp)
    
    for(i in 1:length(stp)){
      sample <- gsub(paste0("\\b",stp[i],"\\b"), "",sample)
    }
    
    sample <- gsub("\\s+"," ", sample)
    
  }
  
  sample <- tolower(sample)
  kw <- stringi::stri_trans_general(kw,"Latin-ASCII")
  sample <- unlist(sample)
  
  sample <- stringi::stri_trans_general(sample,"Latin-ASCII")
  
  ht <- x_wordtree(sample, kw, window = window, direction = direction, height = height)
  
  tp <- tempfile(pattern = "temp", fileext = ".html")
  
  write(ht, tp)
  
  if(url.return==T){
    return(tp)  
  }else{
    browseURL(tp)
  }
  
}
