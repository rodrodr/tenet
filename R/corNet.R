#
#
# This is the function corNet that creates a
# sociogram for the correlation among words 
# containded in a corpus. 
#
#' @import ggplot2
#' @importFrom igraph graph_from_data_frame
#' @importFrom tidygraph as_tbl_graph
#' @import stringi
#' @import ggraph
#' @export
corNet <- function(cor.list, 
                   link.col=c("red","steelblue1"),
                   link.alpha=0.3,
                   link.curvature=0.25,
                   node.col="purple",
                   family="sans",
                   layout="fr"){
  
  cel <- cor.list$edges  
  
  cel$Correlation <- "Negative"
  cel$Correlation[cel$value>0] <- "Positive"
  
  names(cel)[1:2] <- c("from","to")
  
  ver <- cor.list$vertices
  ver$docfreq <- NULL
  names(ver) <- c("vertex.names","Frequency")
  ver$vertex.names <- stringi::stri_trans_general(ver$vertex.names, "Latin-ASCII")
  
  cel$from <- stringi::stri_trans_general(cel$from, "Latin-ASCII")
  cel$to <- stringi::stri_trans_general(cel$to, "Latin-ASCII")
  
  cel$Value <- abs(cel$value)
  
  cel$value <- NULL
  
  cor_g <- igraph::graph_from_data_frame(cel, 
                                         directed = F, 
                                         vertices = ver)
  
  n <- tidygraph::as_tbl_graph(cor_g)
  
  p <- ggraph::ggraph(n, layout="igraph", algorithm=layout)
  
  p <- p + ggraph::geom_edge_arc(aes(edge_width=.data$Value,
                                     color=.data$Correlation),
                                 strength=link.curvature, alpha=link.alpha)
  
  p <- p+ ggraph::scale_edge_color_manual(values = link.col)+
    ggraph::geom_node_point(aes(size = .data$Frequency), color=node.col)+
    ggraph::scale_edge_width_continuous(range = c(0.2, 2))+
    ggraph::geom_node_text(aes(label=.data$name), repel = T, check_overlap = T, 
                           family=family) + 
    theme_void()+ 
    theme(plot.title = ggtext::element_markdown(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5),
                             units = "cm"), 
          text = element_text(family=family))
  
  return(p)
  
}
