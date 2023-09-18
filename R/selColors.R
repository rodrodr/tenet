#
#
# This is the function selColors that generates a list of 
# colors for charts.
#
#' @export

selColors <- function(palette=c("#1B9E77",
                                "#D95F02",
                                "#7570B3",
                                "#E7298A",
                                "#66A61E",
                                "#E6AB02",
                                "#A6761D",
                                "#666666"), 
                      col.n=9){

  if(col.n==length(palette)){
    co <- palette
  }else if(col.n<length(palette)){  
    N <- length(palette)
      ideal <- seq(1,N,(N-1)/(col.n-1))
      co <- palette[round(ideal)] 
        
  }else{
    col <- colorRampPalette(colors = palette)
    co <- col(col.n)
  }

  return(co) 
}
