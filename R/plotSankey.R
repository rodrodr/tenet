#' plotSankey
#'
#' Creates an amCharts Sankey Diagram
#'
#' @import htmlwidgets
#' @import jsonlite
#' 
#' @export
plotSankey <- function(data, from="from", to="to", value="value",
                       font.size=12, opacity=0.05, paddingRight=150, 
                       elementId ="sankeydiv", height=600){
  
  data <- data[,c(from, to, value)]
  names(data) <- c("from","to","value")
  
  data <- jsonlite::toJSON(data)
  
  # forward options using x
  x = list(
    data = data,
    font_size = font.size,
    padRight = paddingRight,
    opacity = opacity
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'plotSankey',
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
#' @name plotSankey-shiny
#'
#' @export
plotSankeyOutput <- function(outputId, width = '100%', height = '600px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plotSankey', width, height, package = 'tenet')
}

#' @rdname plotSankey-shiny
#' @export
renderplotSankey <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, plotSankeyOutput, env, quoted = TRUE)
}




