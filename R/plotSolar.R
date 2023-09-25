# 
# Creates a Solar Centrality Plot
# 
# A Solar Centrality Plot uses the metaphor of an orbital system
# 
#
#' @import igraph
#' @import ggplot2
#' @import ggrepel
#' @import ggforce
#' @import ggtext
#' @importFrom scales rescale
#' 
#' 
#' @export
plotSolar <- function(data, 
                      from="from", 
                      to="to", 
                      value="value",
                      title="**Solar Network Centrality Plot**",
                      subtitle="Relative Centrality and Rank Variation of Nodes in a Network.",
                      caption="Own elaboration using network centrality ranks.",
                      value.lab="Mentions",
                      directed=FALSE,
                      pos.jitter=0,
                      palette=c("#FF0000",
                                "#00A08A",
                                "#F2AD00",
                                "#F98400",
                                "#5BBCD6"),
                      center.col="red2",
                      center.radius=0.05,
                      orbit.line.col="grey90",
                      orbit.line.type="dotted",
                      orbit.line.width=0.5,
                      arc.line.width=0.5,
                      start.line.col="purple",
                      start.line.width=0.5,
                      start.line.type="dotted",
                      legend.position="bottom",
                      legend.justification="left"){
  
  nm <- unique(c(data[,from],data[,to]))
  
  ag <- aggregate(list(value=data[,value]), 
                  by=list(name=data[,to]), 
                  sum)
  
  if(directed==FALSE){
    ag2 <- aggregate(list(value=data[,value]), 
                     by=list(name=data[,from]), sum)
    
    ag <- rbind(ag,ag2)
    ag <- aggregate(list(value=ag$value),
                    by=list(name=ag$name),
                    sum,
                    na.rm=T)
  }  
  
  g <- igraph::graph_from_data_frame(data, 
                                     directed = T, 
                                     vertices = ag)
  
  ag$cn.degree <- rank(-igraph::centr_degree(g)$res)
  ag$cn.between <- rank(-igraph::centr_betw(g)$res)
  ag$cn.closeness <- rank(-igraph::closeness(g, mode = "all"))
  ag$cn.eigen <- rank(-igraph::eigen_centrality(g)$vector)
  ag$cn.pagerank <- rank(-igraph::page_rank(g)$vector)
  
  ag$overall <- rowSums(ag[,c(3:7)])/5
  
  ag$cn.degree.dist <- -igraph::centr_degree(g)$res
  ag$cn.between.dist <- -igraph::centr_betw(g)$res
  ag$cn.closeness.dist <- -igraph::closeness(g, mode = "all")
  ag$cn.eigen.dist <- -igraph::eigen_centrality(g)$vector
  ag$cn.pagerank.dist <- -igraph::page_rank(g)$vector
  
  
  ag$degree.scale <- scales::rescale(ag$cn.degree.dist,
                                     to = c(0.1,1) )
  
  ag$var <- NA
  
  for(i in 1:nrow(ag)){
    ag$var[i] <- stats::sd(as.numeric(ag[i,c(3:7)]))
  }
  
  ag$a <- round(scales::rescale(ag$var, 
                                to=c(360,0), 
                                from=c(0.05,max(ag$var))),1)
  
  ag$r <- ag$degree.scale
  
  ag$ax <- cos(ag$a*pi/180)*ag$r
  ag$ay <- sin(ag$a*pi/180)*ag$r
  
  ag$arc <- scales::rescale(c(ag$var), 
                            to=c(1.5, 7.8), 
                            from=c(0,max(ag$var)))
  
  ag <- ag[order(ag$arc, decreasing = T),]
  
  ag$name <- factor(ag$name)
  
  p <- ggplot2::ggplot()+
    geom_segment(
      aes(x=0, 
          xend=1,
          y=0, 
          yend=0, 
          color=start.line.col), 
      linetype=start.line.type, 
      linewidth=start.line.width)+
    ggforce::geom_circle(
      aes(
        x0=rep(0,nrow(ag)), 
        y0=rep(0,nrow(ag)), 
        r=ag$r),
      linetype=orbit.line.type,
      linewidth=orbit.line.width,
      color=orbit.line.col)+
    ggforce::geom_circle(
      aes(
        x0=0, 
        y0=0, 
        r=center.radius),
      fill=center.col, 
      color=center.col)
  
  p <- p +
    ggforce::geom_arc(
      aes(x0 = rep(0,nrow(ag)),
          y0 = rep(0,nrow(ag)), 
          r = ag$r, 
          start = rep(1.57, nrow(ag)), 
          end = ag$arc,
          color=ag$name), 
      position=position_jitter(width = pos.jitter),
      show.legend = F,
      linewidth=0.5)
  
  p <- p+
    geom_point(data=ag,
               aes(x=.data$ax, 
                   y=.data$ay, 
                   color=.data$name, 
                   size=.data$value))+
    ggrepel::geom_text_repel(data=ag,
                             aes(x=.data$ax, 
                                 y=.data$ay, 
                                 label=.data$name), 
                             size=4)+
    theme_void()+
    theme(legend.position = legend.position, 
          legend.justification = legend.justification, 
          plot.title = ggtext::element_markdown(),
          plot.subtitle = ggtext::element_markdown(),
          plot.caption = ggtext::element_markdown()
    )+
    coord_fixed()+
    labs(title= title,
         subtitle= subtitle, 
         caption= caption)+
    guides(color="none", size=guide_legend(value.lab))
  
  col <- tenet::selColors(palette, col.n=nrow(ag)+1)
  
  p <- p+ scale_color_manual(values=col)
  
  return(p)
  
}
