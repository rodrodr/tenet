#
#
# This is the function tfRatio that calculates the term frequency
# ratio in a series of documents.
#
#
#' @export
tfRatio <- function(text, keyword, threshold=0, return.selected=F){

  we <- stringi::stri_count(text, regex = keyword)
  wt <- stringi::stri_count(text,regex = "\\w+")

  y<- round((we/wt*100)/(sum(we)/sum(wt)*100),2)

  if(return.selection==F){
    return(y)
  }else{
    return(which(y>threshold))
  }
}
