#
#
# This is the function listPalettes that provides a list
# of all palettes in selected color packages.
#
#' @import lisa
#' @import RColorBrewer
#' @import MetBrewer
#' @import rtist
#' @import wesanderson
#' @import ghibli
#' @export
listPalettes <- function(){
  
  li <- as.list(x = names(lisa::lisa))
  names(li) <-names(lisa::lisa)
  
  me <- as.list(x = names(MetBrewer::MetPalettes))
  names(me) <-names(MetBrewer::MetPalettes)
  
  rt <- as.list(x = names(rtist::rtist_palettes))
  names(rt) <-names(rtist::rtist_palettes)
  
  br <- as.list(x = row.names(RColorBrewer::brewer.pal.info))
  names(br) <- row.names(RColorBrewer::brewer.pal.info)
  
  wa <- as.list(x = names(wesanderson::wes_palettes))
  names(wa) <-names(wesanderson::wes_palettes)
  
  gb <- as.list(x = names(ghibli::ghibli_palettes))
  names(gb) <-names(ghibli::ghibli_palettes)
  
  lis <- list(lisa=li,
              MetBrewer=me,
              rtist=rt,
              RColorBrewer=br,
              wesanderson=wa,
              ghibli=gb)
  
  return(lis) 
  
}
