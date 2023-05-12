#
#
# This is the function returnPalette that return the 
# colors belonging to palettes listed in selected R
# packages.
#
#' @import paletteer
#' @import stringi
#' @export
returnPalette <- function(palette="EdwardHopper"){
  lis <- listPalettes()
  x <- grep(paste0("\\b",palette,"\\b"), lis)
  
  if(length(x)==0){
    stop("Error: Palette name not found.")
  }else{
    nm <- names(lis)[x]
    palette <- stringi::stri_trans_general(palette,"Latin-ASCII")
    palette <- gsub(".","_", palette, fixed=T)
    palette <- gsub("-","_", palette, fixed=T)
    co <- paletteer::paletteer_d(paste0(nm,"::",palette))
    return(co)
  }
}
