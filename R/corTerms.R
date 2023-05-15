#
#
# This is the function corTerms that performs
# a correlation analysis of the frequency of words 
# in a corpus. 
#
#' @import reshape2
#' @import quanteda
#' @import quanteda.textstats
#' @importFrom stats cor
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_tail
#' @export
corTerms <- function(corpus, 
                     min.freq=20,
                     lang="es",
                     method="spearman",
                     r.lim=0,
                     n.terms=50,
                     remove.wordlist=NULL){
  
  tx <- quanteda::tokens(corpus, remove_punct = T, remove_symbols = T)
  
  if(! is.null(remove.wordlist)){
    rr <- c(quanteda::stopwords(lang),remove.wordlist)
  }else{
    rr <- quanteda::stopwords(lang)
  }
  
  tx <- quanteda::tokens_remove(tx, rr)
  tx <- quanteda::tokens_tolower(tx)
  fm <- quanteda::dfm(tx)
  fx <- quanteda::dfm_trim(fm, min_termfreq = min.freq)
  xy <- quanteda::convert(fx,to="data.frame")
  
  xm <- cor(xy[,c(2:ncol(xy))], method = method)
  
  xx <- reshape2::melt(xm, varnames = c("term1","term2"))
  
  xx <- xx[xx$value< -r.lim | xx$value > r.lim,]
  xx <- xx[xx$value < 0.9999,]
  
  xx <- xx[order(xx$value),]
  xx <- xx[1:nrow(xx)%%2==1,]
  
  a1 <- xx |> dplyr::slice_head(n=round(n.terms/2))
  b1 <- xx |> dplyr::slice_tail(n=round(n.terms/2))
  
  xx <- unique(rbind(a1,b1))
  
  tm <- as.character(unique(c(xx$term1, xx$term2)))
  
  fq <- quanteda.textstats::textstat_frequency(fx)
  
  fq <- fq[fq$feature%in%tm,c("feature","frequency","docfreq")]
  
  return(list(edges=xx, vertices=fq))
  
}
