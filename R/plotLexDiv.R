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
  
  stopifnot("Only one custom color should be provided. Use a dictionary if you want to represent keywords under different colors." = length(custom.color)==1)
  
  if (!is.null(docvar) & !is.null(value) & !is.null(aggr.by.var)) {
    dx <- data.frame(name = 
                       unique(quanteda::docvars(corpus, 
                                              aggr.by.var)), y = 1)
    data <- filterWordsAttribute(corpus = corpus, docvar = docvar, 
                                 value = value, aggr.by.var = aggr.by.var)
    keywords <- value
  }else {
    dx <- data.frame(name = quanteda::docnames(corpus), y = 1)
    data <- filterWords(corpus = corpus, 
                        keywords = keywords, 
                        lang = lang, 
                        rem.accent = rem.accent, 
                        rem.punct = rem.punct, 
                        case.insensitive = case.insensitive)
  }
  if (na.rm == FALSE) {
    data <- merge(dx, data, by = "name", all.x = T)
  }else {
    data$y <- 1
  }
  
  gp <- which(names(data) == "group")
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$index, y = .data$y))
  
  p <- p + ggplot2::theme(
    axis.line =ggplot2::element_blank(), 
    panel.background = ggplot2::element_rect(fill = panel.bg.fill), 
    panel.grid = ggplot2::element_blank(), 
    plot.background = ggplot2::element_blank(), 
    legend.position = "bottom", legend.box = "vertical", 
    panel.border = ggplot2::element_blank(), 
    axis.ticks = ggplot2::element_blank(), 
    axis.text = ggplot2::element_blank(), 
    axis.title = ggplot2::element_blank(), 
    plot.title = ggtext::element_markdown(), 
    plot.subtitle = ggtext::element_markdown(), 
    panel.spacing = grid::unit(0.1, "lines"), 
    strip.text.y = ggplot2::element_text(angle = 0,hjust = 0), 
    strip.background = ggplot2::element_rect(fill = NA)) + 
    ggplot2::guides(color = ggplot2::guide_legend(nrow = legend.rows, byrow = TRUE))
  
  p <- p + ggplot2::geom_hline(yintercept = 0.5, 
                               color = hline.color, 
                               linewidth = hline.width)

  if (length(gp) > 0) {
    gp <- unique(data$group)
    gp <- gp[!is.na(gp)]
    co <- selColors(palette = palette, col.n = length(gp))
    
    p <- p + ggplot2::geom_segment(ggplot2::aes(xend = .data$index, yend = 0, 
                                                color = .data$group), na.rm = na.rm) + ggplot2::facet_grid(.data$name ~ .)
    
    p <- p + ggplot2::labs(title = title, subtitle = subtitle, caption = caption, 
                           color = legend.title)
    
    p <- p + ggplot2::scale_color_manual(values = co, na.translate = FALSE)

  }else {
    co <- custom.color

    p <- p + ggplot2::geom_segment(ggplot2::aes(xend = .data$index, yend = 0), 
                                   na.rm = na.rm, color = co) + 
      ggplot2::facet_grid(.data$name ~ .)
    
    p <- p + ggplot2::labs(title = title, 
                           subtitle = paste0(subtitle,
                                             ": ", gsub("\\^", "", stringi::stri_flatten(keywords,", "))), caption = caption)
  }
  
  p
  
}
