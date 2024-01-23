# 
# Creates an amCharts Voronoi Tree Diagram
#
#' @import htmlwidgets
#' 
#' @export
plotVoronoiTree <- function(data, 
                            value_col="value", 
                            height=500, 
                            font.size=11, 
                            font.size.parent=25, 
                            hide.parent.label= FALSE, 
                            strokeWidth=2, 
                            strokeWidthParent=5, 
                            strokeColor="#000", 
                            type="polygon", 
                            cornerCount=120, 
                            initialDepth=2, 
                            elementId="voronoitreediv"){
  
  
  cx <- as.character(d3r::d3_nest(data = data, value_cols = value_col))
  
  cx <- gsub(value_col, "value", cx)
  
  data <- htmlwidgets::JS(cx)
  
  if(hide.parent.label==TRUE){
    hide.parent.label <-  htmlwidgets::JS("true")
  }else{
    hide.parent.label <-  htmlwidgets::JS("false")
  }

  # forward options using x
  x = list(
    data = data,
    font_size=font.size,
    font_size_parent=font.size.parent,
    strokeWidth=strokeWidth,
    strokeWidthParent=strokeWidthParent,
    hide_parent=hide.parent.label,
    strokeColor = strokeColor,
    type = type,
    cornerCount = cornerCount,
    initialDepth = initialDepth
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'plotVoronoiTree',
    x,
    width = "100%",
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
#'   \code{'400px'}, \code{'500px'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#'
#' @name plotVoronoiTree-shiny
#'
#' @export
plotVoronoiTreeOutput <- function(outputId, width = '100%', height = '500px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plotVoronoiTree', width, height, package = 'tenet')
}

#' Shiny bindings for mywidget
#'
#' Output and render functions for using mywidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'500px'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#'
#' @name plotVoronoiTree-shiny
#'
#' @export
plotVoronoiTreeOutput <- function(outputId, width = '100%', height = '500px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plotVoronoi', width, height, package = 'tenet')
}

#' @rdname plotVoronoiTree-shiny

#' @param expr An expression that generates a mywidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name renderplotVoronoiTree-shiny
#'
#' @export
renderplotVoronoiTree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, plotVoronoiTreeOutput, env, quoted = TRUE)
}

#' @rdname remderplotVoronoiTree-shiny
