#
#
# This is the function filterWords that filters words from a
# corpus.
#
#' @import stringi
#' @import quanteda
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr n
#' @export
filterWords <- function(corpus,
                        keywords,
                        rem.accent = FALSE,
                        rem.punct = TRUE,
                        case.insensitive = TRUE,
                        lang = "es",
                        fast = TRUE){
  
  group <- NULL
  
  if (quanteda::is.dictionary(keywords)) {
    nm <- names(keywords)
    kw <- vector()
    for (i in 1:length(keywords)) {
      kw <- c(kw, keywords[[i]])
      group <- c(group, rep(nm[i], length(keywords[[i]])))
    }
    keywords <- kw
  }
  
  
  if(fast==TRUE){
    dt <-outer(as.character(corpus), "\\s+", 
               FUN = stringi::stri_split_regex)
    nm <- rep(row.names(dt), lengths(dt))
  }else{
    dt <- quanteda::tokens(as.character(corpus), remove_punct = rem.punct)
    nm <- rep(names(lengths(dt)), lengths(dt))
  }
  
  
  wd <- unlist(dt)
  dt <- data.frame(name = nm, word = wd)
  dt <- dt[!tolower(dt$word) %in% stopwords(lang), ]
  dt <- dt[which(dt$word != " "), ]
  dt <- dt[which(dt$word != ""), ]
  dt <- dt[which(dt$word != "\n"), ]
  
  if (rem.accent == T) {
    dt$word <- stringi::stri_trans_general(dt$word, "Latin-ASCII")
    keywords <- stringi::stri_trans_general(keywords, "Latin-ASCII")
    
  }
  
  if (rem.punct == T) {
    dt$word <- stringi::stri_replace_all(dt$word, regex = "[:punct:]", 
                                         "")
  }
  
  dt <- dplyr::mutate(dplyr::group_by(dt, .data$name), index = dplyr::row_number()/dplyr::n())
  
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
