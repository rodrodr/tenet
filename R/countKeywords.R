#
#
# This is the function countKeywords that counts
# the frequency of terms from a dictionary 
# in a corpus. 
#
#' @import reshape2
#' @import stringi
#' @import quanteda
#' @import collapse
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @export
countKeywords <- function(corpus, dic, group.var=NULL, rel.freq=FALSE, case_insensitive=TRUE){
  
  if(! is.null(group.var)){
    corpus <- quanteda::corpus_group(corpus, 
                                     groups=quanteda::docvars(corpus, group.var))
    gp <- quanteda::docvars(corpus,group.var)
  }
  
  if(rel.freq==T){
    tot <- stringi::stri_count_words(corpus)
  }else{
    tot <- 1
  }
  
  xx <- collapse::unlist2d(dic)
  dp <- collapse::ldepth(dic)
  
  if(dp==1){
    id <- c(".id.1")
    xx$.id.2 <- NULL
    nm <- c("level1", "keyword")
    
  }else if(dp==2){
    id <- c(".id.1",".id.2")
    xx$.id.3 <- NULL
    nm <- c("level1","level2","keyword")
  }else{
    stop("This function only accepts 1 or 2-level dictionaries.")
  }
  
  xx <- reshape2::melt(xx, id.vars =  id)
  xx <- xx[! is.na(xx$value),]
  xx$variable <-NULL
  names(xx) <- nm
  
  pb <- utils::txtProgressBar(min = 0, max=nrow(xx), style=3, char="=", width = 50)
  
  res <- data.frame()
  
  for(i in 1:nrow(xx)){
    
    tf <- stringi::stri_count_regex(corpus, xx$keyword[i], case_insensitive=case_insensitive)
    
    if(! is.null(group.var)){
      res <- rbind(res, 
                   data.frame(
                     keyword=rep(xx$keyword[i], 
                                 length(tf)),
                     groups=gp,
                     frequency=tf/tot))
    }else{
      res <- rbind(res, 
                   data.frame(
                     keyword=xx$keyword[i],
                     frequency=sum(tf)/sum(tot)))
    }
    
    utils::setTxtProgressBar(pb, value = i)
    
  }
  
  xx <- merge(xx, res, by=c("keyword"), all.x=T)
  xx <- xx[! is.na(xx$frequency),]
  
  
  if(! "groups" %in%names(xx)){
    xx$groups <- "All"
  }
  
  
  if(dp==1){
    ag <- aggregate(list(frequency=xx$frequency), 
                    by=list(
                      groups=xx$groups, 
                      level1=xx$level1, 
                      keyword=xx$keyword), 
                    sum, na.rm=T)
  }else{
    ag <- aggregate(list(frequency=xx$frequency), 
                    by=list(
                      groups=xx$groups, 
                      level1=xx$level1, 
                      level2=xx$level2, 
                      keyword=xx$keyword), 
                    sum, na.rm=T)
    
  }
  
  return(ag)
  
}
