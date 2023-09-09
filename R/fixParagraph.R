#
#
# This is the function fixParagraph.
#
#' @import stringi
#' @importFrom readr read_lines
#' @export
fixParagraph <- function(text, check.doubleline=FALSE){
  
  xx <- readr::read_lines(text)
  xx <- trimws(xx)
  xx <- paste0(xx, collapse = "\n")
  
  if (check.doubleline==TRUE){
    db <- "1,2"
  }else{
    db <- "1"
  }
  tt <- stringi::stri_replace_all_regex(
    xx,
    pattern = "([a-zA-Z]{1,})(\\n{1,})([a-zA-Z]{1})", 
    replacement = "$1 $3")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = paste0("([a-zA-Z]{1,})(\\n{",db,"})(\\D)([a-zA-Z]{1})"), 
    replacement = "$1 $3$4")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = paste0("([a-z0-9])([^.])(\\n{",db,"})([a-z0-9]{1})"), 
    replacement = "$1$2 $4")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = paste0("(\\p{L})(\\n{",db,"})(\\p{L})"), 
    replacement = "$1 $3")
  
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = "(\\n{3,})", 
    replacement = "\\\n\\\n")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = "([a-zA-Z0-9])(\\n+)(\\D)", 
    replacement = "$1 $3")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = paste0("([a-zA-Z0-9])([\\.])(\\n{",db,"})([a-z]{1})"), 
    replacement = "$1$2 $4")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = paste0("([a-zA-Z0-9])([\\.])(\\n{",db,"})([a-z]{1})"), 
    replacement = "$1$2 $4")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = paste0("(\\,|\\;|\\||\\?|\\-|\\―|\\—|%)(\\n{",db,"})([a-zA-Z\\p{Latin}]{1})"),
    replacement = "$1 $3")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = paste0("(\\,\\;|\\||\\?|\\!|\\-|\\―|\\—|%)(\\n{",db,"})(\\,|\\;|\\||\\¿|\\¡|\\-|\\―|\\—|%)"),
    replacement = "$1 $3")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = "(\\-|\\;|\\||\\-|\\―|\\—|%)(\\s{1})([a-zA-Z\\p{Latin}]{1})",
    replacement = "$3")
  
  tt <- stringi::stri_replace_all_regex(
    tt,
    pattern = "(\\-|\\;|\\||\\-|\\―|\\—|%)(\\s{1})([a-zA-Z|\\p{Latin}]{1,})",
    replacement = "$3")
  
  return(tt)
  
}

