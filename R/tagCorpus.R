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
                      palette = c("#DD8D29","#E2D200","#46ACC8","#E58601","#B40F20"),
                      bright = 130, 
                      pagination = TRUE, 
                      defaultPageSize=10,
                      show.details=TRUE,
                      case.insensitive=TRUE,
                      remove.accent=TRUE,
                      filter=FALSE,
                      data.return=FALSE,
                      quietly=TRUE){
  
  nm <- names(dic)
  
  if(quietly==FALSE){
    pb <- utils::txtProgressBar(min = 0, max=length(nm), style=3, char="=", width = 50)
  }
  
  if(reshape.dic==TRUE){
    cp <- quanteda::corpus_reshape(corpus, reshape.to)
  }
  
  docs <- docnames(cp)
  
  docs <- stringi::stri_split_fixed(docs, pattern = ".",2, simplify = T)[,1]
  
  text <- as.character(cp)
  
  if (remove.accent==TRUE){
    text <- stringi::stri_trans_general(text, "Latin-ASCII")
  }
  
  col <- selColors(palette=palette, col.n=length(nm))
  
  tcol <- colBright(col, limit = bright)
  
  base <- paste0("<span style='color:", 
                 tcol,";background-color:",
                 col,";padding:5px;border-radius:5px;margin-top:3px;margin-left:3px;display:inline-block;'>")
  
  
  dc <- data.frame()
  
  
  for(i in 1:length(nm)){
    
    ka <- as.character(unlist(dic[i]))  
    
    if (remove.accent==TRUE){
      ka <- stringi::stri_trans_general(ka, "Latin-ASCII")
    }
    
    bb <- paste0("\\b((", paste(ka,collapse = "|"), ")(?:[^\\s]|(?!\\w)){1,})\\b")
    bx <- paste0("<span style='color:", 
                 tcol[i], "; background-color:", 
                 col[i], "'>", "$1", "</span>")    
    
    freq <- stringi::stri_count_regex(text, pattern = bb, case_insensitive=T)
    
    text <- stringi::stri_replace_all_regex(text, pattern = bb, replacement = bx,
                                            opts_regex = stringi::stri_opts_regex(
                                              case_insensitive = case.insensitive), vectorize_all = F)
    
    dc<- rbind(dc, data.frame(cat=nm[i], 
                              order=c(1:length(text)),
                              freq=freq))
    
    dc <- dc[dc$freq>0,]
    
    if(quietly==FALSE){
      utils::setTxtProgressBar(pb, value = i)
    }
    
  }      
  
  da <- aggregate(list(matches=dc$freq), 
                  by=list(order=dc$order), 
                  sum, na.rm=T)
  
  db <- aggregate(list(matches=dc$freq), 
                  by=list(order=dc$order, cat=dc$cat), 
                  sum, na.rm=T)
  
  df <- db
  
  dx <- aggregate(list(ncats=db$order), 
                  by=list(order=db$order), length)
  
  dm <- aggregate(list(max=df$matches), 
                  by=list(order=df$order), max)
  
  df <- merge(df, dm, by="order")
  df <- df[df$matches==df$max,]
  
  df$main_cat_span <- NA
  db$cats_span <- NA
  
  for(i in 1:length(nm)){
    df$main_cat_span[which(df$cat==nm[i])] <- paste0(base[i],df$cat[which(df$cat==nm[i])] ,"</span>") 
    db$cats_span[which(db$cat==nm[i])] <- paste0(base[i],db$cat[which(db$cat==nm[i])] ,"</span>") 
    
  }
  
  
  df <- aggregate(list(main_cat=df$cat, main_cat_span=df$main_cat_span), 
                  by=list(order=df$order), paste0, collapse=" ")
  db <- aggregate(list(cats=db$cat, cats_span=db$cats_span), 
                  by=list(order=db$order), paste0, collapse=" ")
  
  data <- data.frame(docid=docs, order=1:length(text), text)
  
  data <- merge(data, df, by="order", all.x=T)
  data <- merge(data, db, by="order", all.x=T)
  data <- merge(data, da, by="order", all.x=T)
  data <- merge(data, dx, by="order", all.x=T)
  
  data$ncats[is.na(data$ncats)]<-0
  data$matches[is.na(data$matches)]<-0
  
  
  if(filter==TRUE) data <- data[data$ncats>0,]
  
  if(data.return==TRUE){
    
    data$main_cat_span <- NULL
    data$cats_span <- NULL
    
    return(data)
    
  }else{
    
    data$main_cat <- data$main_cat_span
    data$cats <- data$cats_span
    
    data$main_cat_span <- NULL
    data$cats_span <- NULL
    
    data$order <- ave(data$order, data$docid, FUN=seq_along)
    
    reactable::reactable(data,
                         resizable = TRUE,
                         wrap=TRUE,
                         pagination = pagination,
                         defaultPageSize = defaultPageSize,
                         columns = list(
                           docid=reactable::colDef(name="Doc.",
                                                   filterable = T,
                                                   width = 120, 
                                                   show = show.details),
                           order=reactable::colDef(name="Order",
                                                   width = 80, 
                                                   show = show.details),
                           text=reactable::colDef(name="Paragraph",
                                                  filterable = T,
                                                  html = T),
                           main_cat=reactable::colDef(name="Main Category",
                                                      filterable = T,
                                                      html=T, width=140),
                           cats=reactable::colDef(name="All Categories",
                                                  filterable = T,
                                                  html=T, width=140),
                           matches=reactable::colDef(name="Matches",
                                                     filterable = T,
                                                     html=T, width=100, 
                                                     show = show.details),
                           ncats=reactable::colDef(name="Cat. No.",
                                                   filterable = T,
                                                   html=T, width=80, 
                                                   show = show.details)))
  }
  
}

