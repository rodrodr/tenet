#
#
# This is the function pcaScatter that performs
# Principal Component or Correspondence Analysis
# on an corpus and plots the results in a scatter
# plot.
#
#' @import FactoMineR
#' @import quanteda
#' @import quanteda.textstats
#' @import ggplot2
#' @import ggthemes
#' @import ggiraph
#' @import ggtext
#' @export
pcaScatter <- function(corpus, 
                       lang="es",
                       min.freq = 100,
                       n.clusters = 4,
                       interactive = TRUE,
                       type = "pca",
                       title = "Title",
                       caption = "Source: Own elaboration.",
                       alpha = 0.5,
                       palette = NULL){
  
  tb <- quanteda::tokens(corpus, 
                         remove_punct = T, 
                         remove_symbols = T)
  tb <- quanteda::tokens_remove(tb, 
                                c(quanteda::stopwords(lang)))
  fb <- quanteda::dfm(tb)
  fb <- quanteda::dfm_trim(fb, min_termfreq = min.freq)
  
  dd <- quanteda::convert(fb, to = "data.frame")
  
  dm <- t(dd)
  dm <- dm[2:nrow(dm),]
  dm <- matrix(as.numeric(dm),
               ncol = ncol(dm))
  
  colnames(dm) <- quanteda::docid(fb)
  nm <- names(dd)
  nm <- nm[2:length(nm)]
  rownames(dm) <- nm
  
  if(type=="pca"){
    dca <- FactoMineR::PCA(X = dm, ncp = 3, graph = F)
    ind <-dca$ind
    ind <- data.frame(ind)
    ind$word <- row.names(ind)
    names(ind)[1:2] <- c("Dim.1","Dim.2")
    sub <- "Principal Component Analysis"
  }else{
    dca <- FactoMineR::CA(X = dm, ncp = 3, graph = F)
    ind <- dca$row$coord
    ind <- data.frame(ind)
    ind$word <- row.names(ind)
    sub <- "Correspondence Analysis"
  }
  
  cl <- FactoMineR::HCPC(dca, nb.clust = n.clusters, graph = F)
  
  
  ind$freq <- quanteda.textstats::textstat_frequency(fb)$freq
  ind$cluster <- cl$data.clust$clust
  sm <- dca$eig[1:2,]
  
  ind$tool <- paste0(ind$word, "<br>Freq.: ", ind$freq)
  
  if(! is.null(palette)){
    col <- selColors(palette=palette, col.n = n.clusters)
  }else{
    col <- selColors(col.n = n.clusters)
  }
  
  
  p <- ggplot2::ggplot(ind, 
                       aes(x=.data$Dim.1, 
                           y=.data$Dim.2, 
                           size=.data$freq, 
                           col=.data$cluster))+
    ggiraph::geom_point_interactive(
      aes(tooltip=.data$tool, 
          data_id=.data$word), alpha=alpha)+
    ggthemes::theme_clean()+
    theme(legend.position = "none",
          plot.title = ggtext::element_markdown())+
    labs(title = title, subtitle = sub, caption=caption)+
    xlab(paste0("Dim. 1 (",round(sm[3],1),"%)"))+
    ylab(paste0("Dim. 2 (",round(sm[4],1),"%)"))+
    scale_color_manual_interactive(values=col)
  
  if(interactive==T){
    ggiraph::girafe(ggobj = p)
  }else{
    return(p)
  }
}
