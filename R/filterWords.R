#
#
# This is the function filterWords that filters words from a
# corpus.
#
#' @import stringi
#' @import quanteda
#' @import dplyr
#' @export
filterWords <- function(corpus,
                        keywords,
                        rem.accent = TRUE,
                        rem.punct = TRUE,
                        case.insensitive = TRUE,
                        lang = "es"){
  
  group <- NULL
  
  if(quanteda::is.dictionary(keywords)){
    
    nm <- names(keywords)
    
    kw <- vector()
    
    for(i in 1:length(keywords)){
      kw <- c(kw, keywords[[i]])
      group <- c(group, rep(nm[i], length(keywords[[i]])))
    }
    
    keywords <- kw
  }
  
  dt <- data.frame()
  
  dt <- quanteda::tokens(as.character(corpus))
  
  nm <- rep(names(lengths(dt)),lengths(dt))
  wd <- unlist(dt)
  
  dt <- data.frame(name = nm, 
                   word = wd)
  
  if(rem.accent==T){
    dt$word <- stringi::stri_trans_general(dt$word, "Latin-ASCII")
  } 
  if(rem.punct==T){
    dt$word <- stringi::stri_replace_all(dt$word, regex = "[:punct:]", "")
  } 
  
  dt <- dt[which(dt$word!=" "),]
  dt <- dt[which(dt$word!=""),]
  dt <- dt[which(dt$word!="\n"),]
  
  
  dt <- dt |> 
    dplyr::group_by(.data$name) |> 
    dplyr::mutate(index = row_number()/n())
  
  dt <- dt[!tolower(dt$word) %in% stopwords(lang),]
  
  keywords <- paste0("^", keywords)
  
  idx <- vector()
  idg <- vector()
  
  for(i in 1:length(keywords)){
    
    ind <- which(stringi::stri_detect_regex(dt$word, pattern = keywords[i], case_insensitive=case.insensitive)==T)
    
    
    idx <- c(idx, ind)
    
    
    if(! is.null(group)){
      gin <- rep(group[i], length(ind))
      idg <- c(idg, gin)
    }
    
  }
  
  
  dt <- dt[idx,]
  
  if(! is.null(group)) dt$group <- idg
  
  dt
  
}
