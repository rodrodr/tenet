# 
# Creates an amCharts Voronoi Tree Diagram
#
#' @import htmlwidgets
#' 
#' @export
plotVoronoiTree <- function(data, groups="groups", elements="elements", value="value", height=500, font.size=11, font.size.parent=25, hide.parent.label= FALSE, strokeWidth=2, strokeWidthParent=5, strokeColor="#000", type="polygon", cornerCount=120, elementId="voronoitreediv"){
  
  data <- data[,c(groups,elements, value)]
  names(data) <- c("groups","elements","value")
  
  nm <- unique(data$groups)
  
  cs <- vector()
  
  for(i in 1:length(nm)){
    el <- data$elements[data$groups==nm[i]]
    vl <- data$value[data$groups==nm[i]]
    
    tt <- paste0("{'name': '", el, "', 'value': ", vl,"}", collapse = ",\n")
    
    tx <- paste0("{'name': '", nm[i], "', \n'children':[\n", tt, "\n]}")
    
    cs <- c(cs, tx)
  }
  
  cs <- paste0(cs, collapse = ",\n")
  
  cx <- paste0("{\n'children': [\n", cs, "\n]}")
  
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
    cornerCount = cornerCount
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
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a mywidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name plotVoronoiTree-shiny
#'
#' @export
plotVoronoiTreeOutput <- function(outputId, width = '100%', height = '800px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plotVoronoiTree', width, height, package = 'tenet')
}

#' @rdname plotVoronoiTree-shiny
#' @export
renderplotVoronoiTree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, plotVoronoiTreeOutput, env, quoted = TRUE)
}




