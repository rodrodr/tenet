#
#
# This is the function tagCorpus that creates a
# table with tagged texts (divided by sentences, 
# paragraphs, etc.) from a source a corpus. 
#
#' @import reactable
#' @import quanteda
#' @import stringi
#' @importFrom stats aggregate
#' @importFrom stats ave
#' @export
tagCorpus <- function(corpus, 
                      dic,
                      reshape.dic=TRUE,
                      reshape.to="paragraphs",
                      palette = "EdwardHopper",
                      bright = 130, 
                      pagination = TRUE, 
                      defaultPageSize=10){
  
  if(reshape.dic==FALSE){
    cp <- quanteda::corpus_reshape(corpus, reshape.to)
  }
  
  docs <- docnames(cp)
  docs <- stringi::stri_split_fixed(docs, pattern = ".",2, simplify = T)[,1]
  
  text <- as.character(cp)
  
  text <- stringi::stri_trans_general(text, "Latin-ASCII")
  
  nm <- names(dic)
  col <- selColors(palette=palette, col.n=length(nm))
  tcol <- colBright(col, limit = bright)
  
  dc <- data.frame()
  
  for(i in 1:length(nm)){
    
    ka <- as.character(unlist(dic[i]))  
    ka <- stringi::stri_trans_general(ka, "Latin-ASCII")
    
    for(k in 1:length(ka)){
      
      bb <- paste0("\\b(", ka[k], "(?:[^\\s]|(?!\\w)){1,})\\b")
      bx <- paste0("<span style='color:", 
                   tcol[i], "; background-color:", 
                   col[i], "'>", "$1", "</span>")    
      
      freq <- stringi::stri_count_regex(text, pattern = bb, case_insensitive=T)
      
      dc<- rbind(dc, data.frame(cat=nm[i], 
                                order=c(1:length(text)),
                                freq=freq))
      
      text <- stringi::stri_replace_all_regex(text, 
                                              pattern = bb, 
                                              replacement = bx,
                                              case_insensitive = T)   
    }             
  }
  
  da <- aggregate(list(matches=dc$freq), 
                  by=list(order=dc$order), 
                  sum, na.rm=T)
  
  db <- aggregate(list(matches=dc$freq), 
                  by=list(order=dc$order, cat=dc$cat), 
                  sum, na.rm=T)
  
  df <- db[db$matches>0,]
  db <- db[db$matches>0,]
  
  dc <- aggregate(list(ncats=db$order), 
                  by=list(order=db$order), length)
  
  dm <- aggregate(list(max=df$matches), 
                  by=list(order=df$order), max)
  
  df <- merge(df, dm, by="order")
  df <- df[df$matches==df$max,]
  
  base <- paste0("<span style='color:", 
                 tcol,";background-color:",
                 col,";padding:5px;border-radius:5px;margin-top:3px;margin-left:3px;display:inline-block;'>")
  
  df$main_cat <- NA
  db$cats <- NA
  for(i in 1:length(nm)){
    df$main_cat[which(df$cat==nm[i])] <- paste0(base[i],df$cat[which(df$cat==nm[i])] ,"</span>") 
    db$cats[which(db$cat==nm[i])] <- paste0(base[i],db$cat[which(db$cat==nm[i])] ,"</span>") 
    
  }
  
  df <- aggregate(list(main_cat=df$main_cat), 
                  by=list(order=df$order), paste0, collapse=" ")
  db <- aggregate(list(cats=db$cats), 
                  by=list(order=db$order), paste0, collapse=" ")
  
  data <- data.frame(docid=docs, order=1:length(text), text)
  
  data <- merge(data, df, by="order", all.x=T)
  data <- merge(data, db, by="order", all.x=T)
  data <- merge(data, da, by="order", all.x=T)
  data <- merge(data, dc, by="order", all.x=T)
  
  data$ncats[is.na(data$ncats)]<-0
  
  data$order <- ave(data$order, data$docid, FUN=seq_along)
  
  reactable::reactable(data, 
                       pagination = pagination,
                       defaultPageSize = defaultPageSize,
                       columns = list(
                         docid=colDef(name="Doc.",
                                      filterable = T,
                                      resizable = T,
                                      width = 120),
                         order=colDef(name="Order",
                                      width = 80),
                         text=colDef(name="Paragraph", 
                                     filterable = T,
                                     resizable = T,
                                     html = T),
                         main_cat=colDef(name="Main Category",
                                         filterable = T,
                                         resizable = T,
                                         html=T, width=100),
                         cats=colDef(name="Other Categories",
                                     filterable = T,
                                     resizable = T,
                                     html=T, width=80),
                         matches=colDef(name="Matches",
                                        filterable = T,
                                        resizable = T,
                                        html=T, width=100),
                         ncats=colDef(name="Cat. No.",
                                      filterable = T,
                                      resizable = T,
                                      html=T, width=80)))
}
