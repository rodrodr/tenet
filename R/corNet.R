#
#
# This is the function corNet that creates a
# sociogram for the correlation among words 
# containded in a corpus. 
#
#' @import ggplot2
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph as_data_frame
#' @import stringi
#' @import ggnetwork
#' @export
corNet <- function(cor.list, 
                   link.col=c("red","steelblue1"),
                   link.alpha=0.3,
                   link.curvature=0.25,
                   node.col="purple"){
  
  cor_g <- igraph::graph_from_data_frame(cor.list$edges, 
                                         directed = F, 
                                         vertices = cor.list$vertices)
  
  cel <- igraph::as_data_frame(cor_g, 'edges')
  
  cel$Direction <- "Negative"
  cel$Direction[cel$value>0] <- "Positive"
  
  names(cel)[1:2] <- c("from","to")
  
  ver <- cor.list$vertices
  ver$docfreq <- NULL
  names(ver) <- c("vertex.names","Frequency")
  ver$vertex.names <- stringi::stri_trans_general(ver$vertex.names, "Latin-ASCII")
  
  cel$from <- stringi::stri_trans_general(cel$from, "Latin-ASCII")
  cel$to <- stringi::stri_trans_general(cel$to, "Latin-ASCII")
  
  n <- ggnetwork::ggnetwork(cel)
  n <- merge(n, ver, by="vertex.names", all.x=T)
  
  n$Value <- abs(n$value)
  
  p <- ggplot2::ggplot(n, 
                       aes(x = .data$x, 
                           y = .data$y, 
                           xend = .data$xend, 
                           yend = .data$yend)) +
    ggnetwork::geom_edges(
      aes(color=.data$Direction, # Color of links/edges
          linewidth=.data$Value), # Size
      alpha=link.alpha,           # transparency
      curvature = link.curvature)+
    scale_color_manual(
      values=link.col)
  
  p <- p + ggnetwork::geom_nodes(aes(size=.data$Frequency), col=node.col)
  
  p <- p + ggnetwork::geom_nodetext_repel(aes(label = .data$vertex.names))
  
  p <- p + 
    theme_void()
  
  p <- p + theme(plot.title = ggtext::element_markdown(),
                 plot.margin = unit(c(0.5,0.5,0.5,0.5),
                                    units = "cm"))
  
  return(p) 
}
