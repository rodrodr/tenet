#
#
# This is the function tfRatio that calculates the term frequency
# ratio in a series of documents.
#
#' @import stringi
#' @export
tfRatio <- function(text, keyword, threshold=0, return.selected=FALSE, remove.accent=TRUE, identifier="Latin-ASCII"){

  if(remove.accent==T){
    text <- stringi::stri_trans_general(text, identifier)
  }
  
  we <- stringi::stri_count(text, regex = keyword, case_insensitive=T)
  wt <- stringi::stri_count(text,regex = "\\w+", case_insensitive=T)

  y<- round((we/wt*100)/(sum(we)/sum(wt)*100),2)

  if(return.selected==FALSE){
    return(y)
  }else{
    return(which(y>threshold))
  }
}
