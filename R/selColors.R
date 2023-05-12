#
#
# This is the function selColors that generates a list of 
# colors for charts.
#
#' @export
selColors <- function(palette="Dark2", 
                      col.n=9, 
                      custom.pal=NULL){
  
  if (! is.null(custom.pal)){
    col <- colorRampPalette(custom.pal)
  }else{
      col <- returnPalette(palette)
      col <- colorRampPalette(col)
  }
  
  co <- col(col.n)
  
  return(co) 
}
