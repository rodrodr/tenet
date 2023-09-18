#' forceDirectedTree
#'
#' Creates an amCharts Sankey Diagram
#'
#' @import htmlwidgets
#' 
#' @export
forceDirectedTree <- function(json_data, attraction=-5, palette=NULL,col.n=9, show.link=TRUE, height=800, width="100%", max.radius=5, elementId="chartdiv", tooltip.text="{name}: {value}"){
  
  max.radius <- as.character(max.radius)
  
  if(!is.null(palette)){
    co <- selColors(palette = palette, col.n = col.n)
  }else{
    co <- selColors(col.n = col.n)
  }
  
  co <- paste0('am5.color("', co,'")',collapse = ",\n")
  
  colors <- htmlwidgets::JS(co)
  
  data <- paste0("{
    value: 0,
    children: [",json_data, "]}")
  
  data <- htmlwidgets::JS(data)
  
  opacity <- 1
  
  if(show.link==F){
    opacity <- 0
  }
  

  
  # forward options using x
  x = list(
    data = data,
    colors = colors,
    opacity = opacity,
    attraction = attraction,
    max_radius = max.radius,
    tooltipText = tooltip.text
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'forceDirectedTree',
    x,
    width = width,
    height = height,
    package = 'tenet',
    elementId = elementId
  )
  
}


#' Shiny bindings for mywidget
#'
#' Output and render functions for using mywidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a mywidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name forceDirectedTree-shiny
#'
#' @export
forceDirectedTreeOutput <- function(outputId, width = '100%', height = '800px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'forceDirectedTree', width, height, package = 'tenet')
}

#' @rdname forceDirectedTree-shiny
#' @export
renderforceDirectedTree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, forceDirectedTreeOutput, env, quoted = TRUE)
}




