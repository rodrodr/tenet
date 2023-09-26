#
#
# This is the function plotStream.
#
#' @import reshape2
#' @import htmlwidgets
#' @export
plotStream <- function(data, 
                        x=NULL,
                        y=NULL,
                        group = NULL,
                        palette = c("#DD8D29",
                                    "#E2D200",
                                    "#46ACC8",
                                    "#E58601",
                                    "#B40F20"),
                        height=580,
                        elementId="chartdivstream"){
  
  data <- data[,c(x,y,group)]
  
  data <- reshape2::dcast(data,data[,x]~data[,group], value.var = y)
  
  data[is.na(data)] <- 0
  
  nma <- names(data)[2:ncol(data)]
  nm <- names(data)[2:ncol(data)]

  nm <- c("category",paste0("var",1:length(nm)))
  names(data) <-nm
  
  nma <- c("category", nma)

  ser <- paste0("{var: '", nm[2:length(nm)], 
                "', desc: '", nma[2:length(nma)], "'}",
                collapse = ",\n")
  
  ser <- paste0("[", ser, "]")
  
  var <- vector()
  
  for(i in 1:length(nm)){
    if(nm[i]=="category"){
      var <- cbind(var, paste0(nm[i],": '", data[,nm[i]],"'"))
    }else{
      var <- cbind(var, paste0(nm[i],": ", data[,nm[i]]))
    }
  }
  
  vs <- vector()  
  
  for(i in 1:nrow(var)){
    vs <- c(vs, paste0(var[i,], collapse = ", "))
  }
  
  vs <- paste0("{", vs,"}", collapse = ",\n")  
  
  vs <- paste0("[", vs, "]")
  
  pal <- selColors(palette, length(nma)-1)

  amcolor <- paste0('am5.color("', pal, '")', collapse = ",\n")
  
  # forward options using x
  x = list(
    data = htmlwidgets::JS(vs),
    colors=htmlwidgets::JS(amcolor),
    series=htmlwidgets::JS(ser)
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'plotStream',
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
#' @name plotStream-shiny
#'
#' @export
plotStreamOutput <- function(outputId, width = '100%', height = '600px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plotStream', width, height, package = 'tenet')
}

#' @rdname plotStream-shiny
#' @export
renderplotStream <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, plotStreamOutput, env, quoted = TRUE)
}





