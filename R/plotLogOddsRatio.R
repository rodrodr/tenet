# 
# Creates a Weighted Log Odds Ratio Chart
# 
#
#' @import quanteda
#' @import ggplot2
#' @import ggiraph
#' @import reshape2
#' @import tidylo
#' @import ggtext
#' @importFrom scales rescale
#' 
#' 
#' @export
plotLogOddsRatio <- function(corpus, 
                              ref.cat, 
                              comp.cat=NULL, 
                              palette=c("red3","goldenrod1","dodgerblue3"),
                              remove.punct=TRUE,
                              remove.number=TRUE,
                              remove.stopwords=TRUE,
                              use.stem=FALSE,
                              use.bigrams=FALSE,
                              label.dots=TRUE,
                              gray.area=0,
                              exclude.zeros=FALSE,
                              lang="es",
                              title="Weighted log odds ratio vs. Log Frequency",
                              title.text.size=14,
                              interactive=TRUE,
                              return.data=FALSE){
  
  
  tk<- quanteda::tokens(corpus, 
                        remove_symbols = TRUE, 
                        remove_numbers = remove.number,
                        remove_punct = remove.punct)
  
  
  if(remove.stopwords==TRUE){
    tk <- quanteda::tokens_remove(tk,
                                  quanteda::stopwords(
                                    language = lang))
  }
  
  if(use.stem==TRUE){
    tk <- quanteda::tokens_wordstem(tk, language = lang)
  }
  
  
  
  if(use.bigrams==TRUE){
    tk <- quanteda::tokens_ngrams(tk, n = 2)
  }
  
  
  data <- quanteda::dfm_group(
    quanteda::dfm(tk), 
    groups = quanteda::docid(tk))
  
  data <- quanteda::convert(x = data, to = "data.frame")
  data <- reshape2::melt(data)
  
  data <- data[data$value>0,]
  
  if(! is.null(comp.cat)){
    d1 <- data[data$doc_id%in%c(ref.cat,comp.cat),]
  }else{
    d1 <- data
  }
  
  d1$source <- tolower(d1$variable)
  
  d1 <- d1[! is.na(d1$source),]
  
  d1$ref <- 0
  d1$ref[d1$doc_id==ref.cat] <- 1
  
  
  ag <- d1[,c("doc_id","ref","source","value")]
  
  ag <- aggregate(list(freq=ag$value),
                  by=list(doc_id=ag$doc_id,
                          ref=ag$ref,
                          source=ag$source),
                  sum)
  
  bk <- ag %>% 
    tidylo::bind_log_odds(set = .data$ref, 
                          feature = .data$source, 
                          n = .data$freq) %>%
    dplyr::arrange(dplyr::desc(.data$log_odds_weighted))
  
  bk$log_freq <- log(bk$freq)
  
  by <- bk[bk$doc_id==ref.cat,]
  

  by$lodd_scaled <- scale(by$log_odds_weighted)[,1]
  by$log_freq_scaled <- scales::rescale(by$log_freq, to=c(0,1))
  
  cols <- palette
  
  
  if(gray.area==0) cols <- cols[c(1,3)]
  
  by$color <- "B"
  by$color[by$log_odds_weighted<= -gray.area] <-"A" 
  by$color[by$log_odds_weighted>=gray.area] <- "C"
  
  if(exclude.zeros==TRUE){
    by <- by[by$log_freq>0,]
  }
  
  ttp <- "Token: <span style='color:orange;'><b>% s</b></span><br>Frequency: %.0f <br>Log Odds Ratio: %.1f"
  
  
  p <- ggplot2::ggplot(by, 
                       aes(x=.data$log_freq_scaled, 
                           y=.data$log_odds_weighted, 
                           color=.data$color, 
                           size=.data$freq))+
    ggiraph::geom_point_interactive(
      aes(tooltip = sprintf(ttp, 
                            .data$source, 
                            .data$freq, 
                            .data$log_odds_weighted), 
          data_id = .data$source), 
      position=position_jitter(width = 0.02))+
    ggplot2::geom_hline(yintercept = c(-gray.area,gray.area))+
    ggplot2:: theme(legend.position = "none", 
                    axis.title = element_text(hjust = 1),
                    panel.background = element_rect(fill="white"),
                    plot.title = ggtext::element_markdown(size = title.text.size))+
    ggplot2::scale_color_manual(values = cols)+
    ggplot2::xlab("Log Frequency (scaled)")+
    ggplot2::ylab("Log odds ratio w/ prior")+
    labs(title=title)
  
  if(label.dots==TRUE){
    p <- p +
      ggiraph::geom_label_repel_interactive(
        aes(label=.data$source), 
        size=3)
  }
  
  if (return.data==FALSE){
    
    if(interactive==TRUE){
      return(ggiraph::girafe(ggobj = p))
    }else{
      return(p)
    }
    
  }else{
    return(by[,c(1:8)])
  } 
  
}               
