#
#
# This is the function tfRatio that calculates the term frequency
# ratio in a series of documents.
#
#' @import RColorBrewer
#' @import wesanderson
#' @export
selColors <- function(palette="Dark2", 
                      col.n=9, 
                      custom.pal=NULL){
  
  bre <- row.names(RColorBrewer::brewer.pal.info)
  bre.max <- RColorBrewer::brewer.pal.info$maxcolors
  names(bre.max) <- bre
  
  wes <- names(wesanderson::wes_palettes)
  wes.max <- c(7,5,5,5,4,5,5,5,5,4,5,4,4,5,5,4,4,6,5)
  names(wes.max) <- wes
  
  if (! is.null(custom.pal)){
    
    col <- colorRampPalette(custom.pal)
    co <- col(col.n)
    
  }else{
    if(palette%in%bre){
      
      n <- bre.max[names(bre.max)==palette]
      
      col <- colorRampPalette(RColorBrewer::brewer.pal(n, palette))
      
      co <- col(col.n)
      
    }else if(palette%in%wes){
      
      n <- wes.max[names(wes.max)==palette]
      
      col <- colorRampPalette(wesanderson::wes_palette(palette, n))
      
      co <- col(col.n)
    }
    
  }
  
  
  co 
}
