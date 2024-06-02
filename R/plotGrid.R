#
#
# Function plotGrid
# Creates a Grid to plot table frequencies
#' @import ggplot2
#' @import ggiraph
#' @importFrom cowplot theme_minimal_grid
#' @export
plotGrid <- function(data, 
                     x=NULL, 
                     y=NULL, 
                     size=NULL, 
                     color=NULL, 
                     tooltip=NULL, 
                     palette=c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#FFFF33",
                               "#A65628",
                               "#F781BF",
                               "#999999"), 
                     grid.color="grey90",
                     interactive=TRUE, 
                     family="Helvetica", 
                     x.axis.angle=90, 
                     standardize=FALSE, 
                     avg.mark=TRUE, 
                     axis="y", 
                     alpha=TRUE, 
                     width_svg = 9, 
                     height_svg = 6, 
                     leg.size="Size",
                     leg.color="Dimension", 
                     point.shape="circle"){
  
  if(point.shape=="circle"){
    point.shape <- 19
    pt.bord <- 21
  }else if(point.shape=="square"){
    point.shape <- 15
    pt.bord <- 22
  }else if(point.shape=="triangle"){
    point.shape <- 17
    pt.bord <- 24
  }else if(point.shape=="diamond"){  
    point.shape <- 18
    pt.bord <- 23
  }else{
    point.shape <- 19
    pt.bord <- 21
  }  
  
  if(axis=="y"){
    axis <- y
  }else{
    axis <- x
  }
  
  if(is.null(color)){
    data$color <- "cor"
    color <- "color"
  }
  
  ag <- aggregate(list(avg= data[[size]]), 
                  by=list(x=data[[axis]]), 
                  mean, 
                  na.rm=T)
  
  names(ag)[1] <- axis
  
  data <- merge(data, ag, by=axis)
  
  data$base_100 <- data[[size]]/data[["avg"]]*100
  
  
  if(standardize==TRUE){
    
    # Media igual a 100%
    data$avg <- 100
    
    data[[size]] <- round(data$base_100,1)
    
    if(alpha==T){
      data$alpha = data[[size]]
    }else{
      data$alpha = 1
    }
    
  }else{
    
    data$alpha = 1
  }
  
  if(is.null(tooltip)){
    tooltip <- data[[size]]
  }
  
  p <- ggplot2::ggplot(data,
                       ggplot2::aes(
                         y=.data[[y]], 
                         x=.data[[x]], 
                         size = .data[[size]], 
                         color = .data[[color]],
                         alpha = .data[["alpha"]]))
  
  p <- p + cowplot::theme_minimal_grid(color = grid.color)
  
  p <- p + ggiraph::geom_point_interactive(
    ggplot2::aes(tooltip=tooltip),
    shape=point.shape)+ 
    ggplot2::scale_radius(range = c(0, 10), 
                          trans = "sqrt")
  
  
  
  if(standardize==TRUE | avg.mark==TRUE){
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y=.data[[y]],
                   x=.data[[x]],
                   size=.data[["avg"]]), 
      shape=pt.bord, 
      color="black", 
      stroke=0.5)
  }
  
  p <- p + ggplot2::theme(legend.position="left",
                          axis.text.y = ggplot2::element_text(family = family),
                          axis.text.x = ggplot2::element_text(angle = x.axis.angle, 
                                                              family =family,
                                                              vjust = 0.5, 
                                                              hjust=1))+
    ggplot2::xlab("")+
    ggplot2::ylab("")+
    ggplot2::labs(size=leg.size)
  
  
  p <- p + ggplot2::scale_color_manual(name=leg.color,values= palette)
  
  p <- p + ggplot2::guides(alpha="none", color = ggplot2::guide_legend(override.aes = list(size = 3))) 
  
  
  if(interactive==T){
    p <- ggiraph::girafe(ggobj = p, code = print, width_svg = width_svg, height_svg = height_svg)
  } else {
    p <- p
  }
  
  p
  
}
