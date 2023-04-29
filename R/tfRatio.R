#
#
# This is the function tfRatio that calculates the term frequency
# ratio in a series of documents.
#
#' @import stringi
#' @export
tfRatio <- function(text, keyword, return.selected=FALSE, threshold=0){

  we <- stringi::stri_count(text, regex = keyword)
  wt <- stringi::stri_count(text,regex = "\\w+")

  y<- round((we/wt*100)/(sum(we)/sum(wt)*100),2)

  if(return.selected==F){
    return(y)
  }else{
    return(which(y>threshold))
  }
}
