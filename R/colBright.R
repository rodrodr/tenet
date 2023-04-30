#
#
# Function colBright
# Determine the brightness of background colors to adapt labels
# Dark colors would require white labels and light colors
# would produce black labels
#' @import grDevices
#' @export
colBright <- function(colors, limit=130, bright="white", dark="black"){
  

  rgb <- grDevices::col2rgb(colors)
  
  for(i in 1:ncol(rgb)){
    val <- 0.2126*rgb[1,i]+0.7152*rgb[2,i]+0.0722*rgb[3,i]
    if(i==1) valc <- val else valc <- c(valc,val)
  }
  
  col <- rep(bright, length(colors))
  col[valc>limit] <- dark
  
  col
  
}
