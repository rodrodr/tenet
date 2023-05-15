#
#
# This is the function filterWords that filters words from a
# corpus.
#
#' @import quanteda
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr n
#' @export
filterWordsAttribute <- function(corpus,
                                 docvar,         
                                 value,
                                 aggr.by.var){
  
  nma <- aggr.by.var
  nmd <- docvar
  dt <- quanteda::tokens(as.character(corpus))
  
  docvar <- quanteda::docvars(corpus, docvar)
  aggr.by.var <- quanteda::docvars(corpus, aggr.by.var)
  
  len <- lengths(dt)
  
  
  dt <- data.frame(name = rep(aggr.by.var, len), 
                   word = rep(docvar, len))
  
  
  dt <- dt |> 
    dplyr::group_by(.data$name) |> 
    dplyr::mutate(index = row_number()/n())
  
  dt <- dt[dt$word%in%value,]
  
  if(length(value)>1){
    
    dt$group <- dt$word
  }
  
  dt
  
  
}
