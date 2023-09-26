#
#
# This is the function filterWords that filters words from a
# corpus.
#
#' @import stringi
#' @import ggplot2
#' @import ggtext
#' @import quanteda
#' @export
plotLexDiv <- function(corpus,
                       keywords,
                       docvar=NULL,         
                       value=NULL,
                       aggr.by.var=NULL,
                       rem.accent = TRUE,
                       rem.punct = TRUE,
                       case.insensitive = TRUE,
                       lang = "es", 
                       title="Lexical Dispersion Plot", 
                       caption="Own elaboration.",
                       subtitle="Keywords",
                       legend.title="Group",
                       legend.rows=1,
                       palette=c("#DD8D29","#E2D200","#46ACC8","#E58601","#B40F20"),
                       custom.color="black",
                       panel.bg.fill="grey98",
                       hline.color="white",
                       hline.width=0.5,
                       na.rm=FALSE){
  
  if(! is.null(docvar) & ! is.null(value) & ! is.null(aggr.by.var)){
    
    dx <- data.frame(name=unique(quanteda::docvars(corpus,aggr.by.var)), 
                     y=1)
    
    data <- filterWordsAttribute(corpus = corpus, 
                                 docvar = docvar, 
                                 value = value, 
                                 aggr.by.var = aggr.by.var)
    
    keywords <- value
    
  }else{
    
    dx <- data.frame(name=quanteda::docnames(corpus), y=1)
    
    data <- filterWords(corpus = corpus, 
                        keywords = keywords, 
                        lang = lang, 
                        rem.accent = rem.accent, 
                        rem.punct = rem.punct, 
                        case.insensitive = case.insensitive)
  }
  
  
  if(na.rm==FALSE){
    data <- merge(dx, data, by="name", all.x=T)
  }else{
    data$y <- 1
  }
  
  
  gp <- which(names(data)=="group")
  
  p <- ggplot2::ggplot(data, aes(x = .data$index, y = .data$y))
  
  p <- p + theme(axis.line = element_blank(),
                 panel.background = element_rect(
                   fill = panel.bg.fill),
                 panel.grid = element_blank(),
                 plot.background = element_blank(),
                 legend.position = "bottom",
                 legend.box="vertical",
                 panel.border = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 plot.title = ggtext::element_markdown(),
                 plot.subtitle = ggtext::element_markdown(),
                 panel.spacing = grid::unit(0.1, "lines"),
                 strip.text.y = element_text(angle = 0, hjust = 0),
                 strip.background = element_rect(fill = NA)
  ) +
    guides(color = guide_legend(nrow = legend.rows, byrow = TRUE))
  
  p <- p+ geom_hline(yintercept = 0.5, 
                     color=hline.color, 
                     linewidth=hline.width)
  
  if(length(gp)>0){
    
    gp <- unique(data$group)
    gp <- gp[! is.na(gp)]
    
    
    co <- selColors(palette=palette, col.n = length(gp))

    p <- p + geom_segment(aes(xend = .data$index, yend = 0, color=.data$group), na.rm = na.rm)+
      facet_grid(.data$name~.)
    
    p <- p + labs(title = title,
                  subtitle = subtitle,
                  caption = caption,
                  color= legend.title)
    
    p <- p + scale_color_manual(values= co, na.translate = FALSE)
    
    
  }else{
    
    co <- custom.color

    p <- p + geom_segment(aes(xend = .data$index, yend = 0), 
                          na.rm = na.rm, color=co)+
      facet_grid(.data$name~.)
    
    p <- p + labs(title = title,
                  subtitle = paste0(subtitle, ": ", 
                                    gsub("\\^","",
                                stringi::stri_flatten(keywords, ", "))),
                  caption = caption)
    
    
    
  }
  
  p 
  
  
}
