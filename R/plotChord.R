#' plotChord
#'
#' Creates an amCharts Chord Diagram
#'
#' @import htmlwidgets
#' @import jsonlite
#' 
#' @export
plotChord <- function(data, from="from", to="to", value="value", font.size=12, node.width=5, opacity=0.05, radius.percent=70, height = 600, elementId = "chordtheme") {

  data <- data[,c(from, to, value)]
  names(data) <- c("from","to","value")

  data <- jsonlite::toJSON(data)

  # forward options using x
  x = list(
    data = data,
    font_size = font.size,
    node_width = node.width,
    opacity = opacity,
    radius_percent = radius.percent
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'plotChord',
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
#' @name plotChord-shiny
#'
#' @export
plotChordOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plotChord', width, height, package = 'amChartsWidget')
}

#' @rdname plotChord-shiny
#' @export
renderplotChord <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, mywidgetOutput, env, quoted = TRUE)
}


