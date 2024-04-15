#
#
# This is the function plotKeyness that creates a
# more detailed plot to the results of the keyness 
# method for keyword identification in texts. 
#
#' @import ggplot2
#' @import quanteda
#' @import quanteda.textstats
#' @import ggiraph
#' @importFrom scales rescale
#' @importFrom ggtext element_markdown
#' @export
plotKeyness <- function(corpus,
                        group.var=NULL,
                        ref.cat,
                        p.value=0.01,
                        type="chi2",
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
                        title="Chi-Square vs. Log Frequency",
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
    tk <- quanteda::tokens_wordstem(tk, 
                                    language = lang)
  }
  
  
  
  if(use.bigrams==TRUE){
    tk <- quanteda::tokens_ngrams(tk, n = 2)
  }
  
  pfm <- dfm(tk)
  
  if(is.null(group.var)){
    pfm <- quanteda::dfm_group(pfm, 
                               groups = quanteda::docid(corpus))
  }else{
    pfm <- quanteda::dfm_group(pfm, 
                               groups = quanteda::docvars(
                                                corpus,
                                                group.var))
  }
  
  if (type=="chi2"){
    data <- quanteda.textstats::textstat_keyness(pfm, 
                                                 target = ref.cat, 
                                                 measure = "chi2")
    
    data <- data[data$p<=p.value,]
    
    data$y.value <- data$chi2
    data$x.value <- scales::rescale(data$n_target, 
                                    to = c(0, 1))
    
    ylabel <- c("Chi-Square")
    
  }else if (type=="lr"){
    data <- quanteda.textstats::textstat_keyness(pfm, 
                                                 target = ref.cat, 
                                                 measure = "lr")
    
    data <- data[data$p<=p.value,]
    
    data$y.value <- data$G2
    data$x.value <- scales::rescale(data$n_target, 
                                    to = c(0, 1))
    
    ylabel <- c("Likelihood Ratio (G2)")
    
  }else{
    data <- quanteda.textstats::textstat_keyness(pfm, 
                                                 target = ref.cat, 
                                                 measure = "chi2")
    
    data <- data[data$p<=p.value,]
    
    data$y.value <- log((data$n_target+0.5)/(data$n_reference+0.5))
    data$x.value <- scales::rescale(data$n_target, 
                                    to = c(0, 1))
    
    ylabel <- c("Log Odds Ratio")
    
    
  }  
  
  cols <- palette
  
  
  if(gray.area==0) cols <- cols[c(1,3)]
  
  data$color <- "B"
  data$color[data$y.value<= -gray.area] <-"A" 
  data$color[data$y.value>=gray.area] <- "C"
  
  
  ttp <- paste0("Token: <span style='color:orange;'><b>% s</b></span><br>Freq. target: %.0f <br>Freq. reference: %.0f <br>"
                ,ylabel, 
                ": %.1f")
  
  
  
  p <- ggplot2::ggplot(data, 
                       ggplot2::aes(x=.data$x.value, 
                                    y=.data$y.value, 
                                    color=.data$color, 
                                    size=.data$n_reference))+
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = sprintf(ttp, 
                                     .data$feature, 
                                     .data$n_target,
                                     .data$n_reference,
                                     .data$y.value), 
                   data_id = .data$feature), 
      position=ggplot2::position_jitter(width = 0.02))+
    ggplot2::geom_hline(yintercept = c(-gray.area,gray.area))+
    ggplot2::theme(legend.position = "none", 
                   axis.title = ggplot2::element_text(hjust = 1),
                   panel.background = ggplot2::element_rect(fill="white"),
                   plot.title = ggtext::element_markdown(size = title.text.size))+
    ggplot2::scale_color_manual(values = cols)+
    ggplot2::xlab("Log Frequency (scaled)")+
    ggplot2::ylab(ylabel)+
    ggplot2::labs(title=title)
  
  if(label.dots==TRUE){
    p <- p +
      ggiraph::geom_label_repel_interactive(
        ggplot2::aes(label=.data$feature), 
        size=3)
  }
  
  if (return.data==FALSE){
    
    if(interactive==TRUE){
      return(ggiraph::girafe(ggobj = p))
    }else{
      return(p)
    }
    
  }else{
    
    data <- data[,c("feature","x.value","y.value","p","n_target","n_reference")]
    
    return(data)
  } 
  
}               




