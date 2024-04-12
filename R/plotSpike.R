#
#
# This is the function plotSpike that creates both
# classical and circular lexical dispersion plots 
# for large number of documents
#
#' @import ggplot2
#' @import ggiraph
#' @export
plotSpike <- function(data=NULL, 
                      palette=c("#017a4a",
                                "#9F248F",
                                "#FFCE4E",
                                "#244579",
                                "#c6242d"), 
                      doc_id="name",
                      index_var="index",
                      word_var="word",
                      group_var="group",
                      top_n=NULL, 
                      sort=FALSE,
                      polar=TRUE, 
                      quartiles=FALSE, 
                      text_label=NULL, 
                      tooltip_values=NULL, 
                      tooltip_doc="name",
                      label.size=1,
                      ring.col="red3",
                      line.width=0.1,
                      legend.position="top",
                      legend.title="Group",
                      title="Lexical Spike Plot",
                      subtitle="Keyword dispersion in texts.",
                      svg.height=5,
                      svg.width=6,
                      ncol=NULL,
                      nrow=NULL,
                      interactive=TRUE){
  
  
  stopifnot("The data is not specified. Please provide a valid data frame containing at least the document id, words, and their relative positions or indexes." = is.null(data)==FALSE)
  # 
  stopifnot("The parameter doc_id, containing the variable corresponding to the document id in the data, is not specified. Please provide a valid variable name." = is.null(doc_id)==FALSE)
  
  
  # if(is.null(data)){
  #   
  #   return("The data is not specified. Please provide a valid data frame containing at least the document id, words, and their relative positions or indexes.")
  #   
  # }
  # 
  # if(is.null(doc_id)){
  #   
  #   return("The parameter doc_id, containing the variable corresponding to the document id in the data, is not specified. Please provide a valid variable name.")
  #   
  # }
  
  
  nm <- names(data)
  
  if(! doc_id %in% nm){
    
    return(paste0("The variable '", doc_id, "' is not found in the data. Please provide a valid variable name."))
    
  }
  
  if(! word_var %in% nm){
    
    return(paste0("The variable '", word_var, "' is not found in the data. Please provide a valid variable name."))
    
  }
  
  if(! index_var %in% nm){
    
    return(paste0("The variable '", index_var, "' is not found in the data. Please provide a valid variable name."))
    
  }
  
  
  data$name <- data[[doc_id]]
  
  data$word <- data[[word_var]]
  
  data$index <- data[[index_var]]
  
  ag <- aggregate(
    list(
      oc=data$name), 
    by=list(
      name=data$name), 
    FUN=length)
  
  ag <- ag[order(ag$oc,decreasing = T),]
  
  
  if(! is.null(top_n)){
    ag <- ag[1:top_n,]
  }
  
  data <- merge(data, ag, by=doc_id)
  
  if(interactive==TRUE){
    size <- label.size
  }else{
    size <- label.size*2
  }
  
  if(! is.null(group_var)){
    
    data$group <- data[,group_var]
    
    ag2 <- aggregate(list(gp=data$group), 
                     by=list(group=data$group, 
                             name=data$name), 
                     FUN=length)
    
    
    data <- merge(data, ag2, by=c("name","group"))
    
    data$per_group <- round(data$gp / data$oc*100,1)
    
    
    segm <- ggiraph::geom_segment_interactive(
      ggplot2::aes_string(x = "index", 
                   xend="index", 
                   y = 0,
                   yend=1, 
                   tooltip="label_values", 
                   color="group"), 
      linewidth=line.width)
  }else{
    segm <- ggiraph::geom_segment_interactive(
      ggplot2::aes_string(x = "index", 
                   xend="index", 
                   y = 0,
                   yend=1, 
                   tooltip="label_values"), 
      linewidth=line.width, 
      color=palette[1])
    
  }
  
  if(sort==TRUE){
    data <- data[order(data$oc, decreasing = T),]
    
    data$name <- factor(data$name, 
                        levels = data$name, 
                        labels = data$name)
  }
  
  
  if(is.null(text_label)){
    data$label_text <- data$oc
  }else{
    data$label_text <- data[[text_label]]
  }
  
  if(is.null(tooltip_values)){
    
    if(is.null(group_var)){
      data$label_values <- paste0("Document: ", data$name,"\n",
                                  "Word: ", data$word)
    }else{
      data$label_values <- paste0("Document: ", data$name,"\n",
                                  "Group: ", data$group,
                                  " (", data$gp," / ", 
                                  data$per_group, "%)\n",
                                  "Word: ", data$word)
    }
  }else{
    data$label_values <-data[[tooltip_values]]
  }
  
  
  if(is.null(tooltip_doc)){
    data$label_doc <- paste0("Document: ", data$name,"\n",
                             "Matches: ", data$oc, " tokens.")
  }else{
    data$label_doc <-data[[tooltip_doc]]
  }
  
  df <- data.frame(x = 0, xend = 0, y = 0, yend = 1)
  
  if(polar==TRUE){
    adj <- 0.5
    yadj <- 0
    pc <- ggplot2::theme(axis.text = ggplot2::element_blank())
    geo <- ggplot2::coord_polar()
  }else{
    pc <- ggplot2::coord_cartesian(clip = 'off')
    geo <-ggplot2::geom_segment(data=df,
                      ggplot2::aes_string(
                                  x = "x", 
                                  xend="xend", 
                                  y = "y",
                                  yend="yend"), 
                                  linewidth=line.width, 
                                  color="black")
    adj <- -0.2
    yadj <- 0.1
  }
  
  
  p <- ggplot2::ggplot(data, 
                       ggplot2::aes(x=index, y=1))+ 
    segm
  
  p <- p +
    ggplot2::geom_segment(data=df,
                          ggplot2::aes_string(
                            x = "x", 
                            xend="xend", 
                            y = "y",
                            yend="yend"), 
                          linewidth=line.width, 
                          color="black")
  
  if(quartiles==TRUE){
    
    dq <- data.frame(x=c(0.25,0.5,0.75), 
                     xend=c(0.25,0.5,0.75), 
                     y=c(0,0,0), 
                     yend=c(1,1,1))
    
    p <- p +   
      ggplot2::geom_segment(data=dq,
        ggplot2::aes_string(x = "x", 
                     xend="xend", 
                     y = "y",
                     yend="yend"), 
        linewidth=line.width, 
        color="black")
    
  }
  
  p <- p +
    ggiraph::geom_rect_interactive(
      mapping=ggplot2::aes_string(
        xmin=0, 
        xmax=1, 
        ymin=0, 
        ymax=0.5, 
        tooltip="label_doc"), 
      fill="white")  +
    ggplot2::geom_text(
      ggplot2::aes_string(label = "label_text"),
      x = 0, 
      y = yadj, 
      hjust=adj,
      size = size,
      lineheight = 0.87,
      color = "black"
    )+
    ggplot2::geom_hline(yintercept=0.5, size=0.2, color=ring.col)+
    ggplot2::theme_void()+
    ggplot2::facet_wrap(~name, shrink = TRUE,ncol=ncol, nrow=nrow)+ 
    ggplot2::theme(
      legend.position = legend.position,
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0,"lines")
    )
  
  p <- p + pc + geo
  
  if(! is.null(palette)){
    p <- p + ggplot2::scale_color_manual(name=legend.title, values = palette)
  }
  
  
  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth=2)))
  
  p <- p+ggplot2::labs(title = title,
                       subtitle = subtitle)
  
  if(interactive==TRUE){
    x <- ggiraph::girafe(ggobj = p, width_svg=svg.width, height_svg=svg.height)
    
    x <- ggiraph::girafe_options(x,
                                 ggiraph::opts_zoom(min = 1, max = 4) )
    
    return(x)
    
  }else{
    
    return(p)
    
  }
  
}
