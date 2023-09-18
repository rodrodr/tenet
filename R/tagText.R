#
#
# Function tagText
# Tags a text according to keywords or quanteda dictionaries
#' @import stringi
#' @import quanteda
#' @import rstudioapi
#' @import utils
#' @export
tagText <- function(text, 
                    title="Document Title", 
                    keywords=NULL,
                    palette=NULL, 
                    bright=130, 
                    tooltip=TRUE, 
                    html.return=FALSE, 
                    viewer=TRUE, 
                    url.return=FALSE, 
                    margin=50, 
                    font.size=12){
  
  if (class(keywords)[1]=="dictionary2"){
    dic <- keywords
    
    if(! is.null(palette)){
      col <- selColors(palette=palette, col.n = length(dic))
    }else{
      col <- selColors(col.n = length(dic))
    }
    
  }else{
    dic <- NULL
    kw <- keywords
    if(! is.null(palette)){
      col <- selColors(palette=palette, col.n = length(kw))
    }else{
      col <- selColors(col.n = length(kw))
    }
  }
  
  txt <- stringi::stri_trans_general(text, "Latin-ASCII")
  
  tcol <- colBright(col, limit = bright)
  
  txt <- gsub(pattern = "\n"," <br>", txt, 
              ignore.case = T)
  
  kz <- vector()
  
  if(! is.null(dic)){
    
    kwd <- vector()
    cof <- vector()
    txcol <- vector()
    tcof <- vector()
    tp <- vector()
    
    nmd <- attributes(dic)$names
    
    if(length(col)==1){
      
      col <- rep(col, length(nmd))
      txcol <- rep(tcol, length(nmd))
      
    }else if (length(col)<length(nmd)){ 
      
      col <- rep("red", length(nmd))
      txcol <- rep("white", length(nmd))
      warning("The number of colors is smaller than the categories in the dictionary. Red was used for all terms in the dictionary.")
      
    }
    
    for(i in 1:length(nmd)){
      
      ka <- as.character(unlist(dic[i]))
      
      cf <- rep(col[i], length(ka)) 
      tcof <- rep(tcol[i], length(ka))
      tnm <- rep(nmd[i], length(ka))
      
      kwd <- c(kwd,ka)
      cof <- c(cof,cf)
      txcol <- c(txcol,tcof)
      tp <- c(tp,tnm)
      
    }
    
    
    kw <- kwd
    
    tu <- unique(tp)
    
    
    ky <- paste0("KEYKUE",1:length(unique(tp)))
    
    
    for(i in 1:length(tu)){
      kz <- c(kz, rep(ky[i], length(tp[tp==tu[i]])))  
    }
    
    
  }else{
    
    if(length(kw)>1 & length(col)==1){
      cof <- rep(col,length(kw)) 
      txcol <- rep(tcol, length(kw))
    }else if(length(col)<length(kw)){ 
      cof <- rep("red", length(kw))
      txcol <- rep("white", length(kw))
      warning("The number of colors is smaller than the categories in the dictionary. Red was used for all terms in the dictionary.")
    }else{ 
      cof <- col
      txcol <- rep(tcol, length(kw))
    }
    
    kz <- ""
    
  }
  
  kw <- stringi::stri_trans_general(kw, "Latin-ASCII")
  
  for(i in 1:length(kw)){
    
    bb <- paste0("\\b(", kw[i],"(?:[^\\s]|(?!\\w)){1,})\\b")
    
    if(tooltip==T & ! is.null(dic)){
      bx <- paste0("<div class='tooltip' style='color:",txcol[i],"; background-color:",cof[i],"'>","$1","<span class='tooltiptext' style='color:",txcol[i],"; background-color:",cof[i],"'>",kz[i],"</span></div>")
    }else{
      bx <- paste0("<span style='color:",txcol[i],"; background-color:",cof[i],"'>","$1","</span>")
      
    }
    
    txt <- stringi::stri_replace_all_regex(txt, pattern = bb, replacement = bx, case_insensitive=T)
    
  }
  
  if(! is.null(dic)){
    for(i in 1:length(ky)){
      txt <- stringi::stri_replace_all_fixed(txt, pattern = ky[i], replacement = tu[i])
    }
  }
  
  
  if(tooltip==F){
    ht <- paste0("<!DOCTYPE html>
  <html>
    <head></head>
    <body style='margin: ", margin," ", margin,"; font-size: ",font.size,"px'><h1>", title,"</h1><br><br>",
  paste0(txt,collapse=" <br>"),"
    </body></html>")
  }else{
    
    ht <- paste0("<style>body {margin: ", margin," ", margin,";font-size: ", font.size, "px;}.tooltip {
  position: relative;
  display: inline-block;
  border-bottom: 1px dotted black;
}.tooltip .tooltiptext {
  visibility: hidden;
  width: 120px;
  background-color: #555;
  color: #fff;
  text-align: center;
  border-radius: 6px;
  padding: 5px 0;
  position: absolute;
  z-index: 1;
  bottom: 125%;
  left: 50%;
  margin-left: -60px;
  opacity: 0;
  transition: opacity 0.3s;
}.tooltip .tooltiptext::after {
  content: '';
  position: absolute;
  top: 100%;
  left: 50%;
  margin-left: -5px;
  border-width: 5px;
  border-style: solid;
  border-color: #555 transparent transparent transparent;
}.tooltip:hover .tooltiptext {
  visibility: visible;
  opacity: 1;
}</style><body><h1>", title,"</h1><br><br>",paste0(txt, collapse="<br>"),"</body></html>")
  }
  
  tp <- tempfile(fileext = ".html")
  
  write(ht, tp)
  
  if(html.return==F & url.return==F){
    if(viewer==T){
      rstudioapi::viewer(tp)
    }else{
      utils::browseURL(tp)
    }
  }else if(html.return==F & url.return==T){
    return(tp)
  }else{
    return(ht)
  }
  
}


