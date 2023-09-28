#
#
# This is the function forceClusTree.
#
#' @import stringi
#' @import quanteda
#' @import quanteda.textstats
#' @import htmlwidgets
#' @export
forceClusTree <- function(corpus, 
                           remove_punct=TRUE,
                           lang = "es",
                           palette = c("#DD8D29",
                                       "#E2D200",
                                       "#46ACC8",
                                       "#E58601",
                                       "#B40F20"),
                           groupvar = NULL,
                           width="100%",
                           height=580,
                           link.width=2,
                           maxRadius=30,
                           BodyStrength=-10,
                           include.text=FALSE,
                           img.docvar=NULL,
                           clust.method="euclidean",
                           weight.scheme="logcount",
                           elementId="chartdivclus"){
  
  
  # parte 1 - cluster
  tk <- quanteda::tokens(corpus, 
                         remove_punct = remove_punct) |> 
    quanteda::tokens_remove(quanteda::stopwords(lang)) |> 
    quanteda::dfm() |> 
    quanteda::dfm_trim(min_termfreq = 5, 
                       min_docfreq = 3)
  
  tkd <- quanteda::dfm_weight(tk, scheme = weight.scheme) %>%
    quanteda.textstats::textstat_dist(method = clust.method) %>% 
    as.dist()
  
  tkc <- hclust(tkd)
  
  # parte 2 - de cluster a df
  dt <- hclust2df(tkc)
  
  nn <- which(is.na(dt$parentId))
  
  nn <- dt$nodeId[nn-1]
  
  dt$parentId[is.na(dt$parentId)] <- nn
  
  if(! is.null(img.docvar)){
    poster <- quanteda::docvars(corpus, img.docvar)
  }else{
    poster <- ""
  }
  
  dx <- data.frame(nodeId=quanteda::docnames(corpus), size=rowSums(tk), poster=poster)
  
  
  if(include.text==T & ! is.null(img.docvar)){
    dx$text <- as.character(corpus) 
    dx$text <- gsub('"',"'", dx$text, fixed = T)
    
    for (i in 1:nrow(dx)){
      dx$text[i] <- paste(stringi::stri_wrap(dx$text[i], 60, 0.0, ), collapse = "<br>")
    }
    
    text.node <- "<p><b>{name}:</b><br><br>{text}<br><br><img src='{poster}' width='300' height='450' style='display: block;margin-left: auto;margin-right: auto;'></p>"
    
  }else if (include.text==T & is.null(img.docvar)){
    
    dx$text <- as.character(corpus) 
    dx$text <- gsub('"',"'", dx$text, fixed = T)
    
    for (i in 1:nrow(dx)){
      dx$text[i] <- paste(stringi::stri_wrap(dx$text[i], 60, 0.0, ), collapse = "<br>")
    }
    
    text.node <- "<p><b>{name}:</b><br><br>{text}</p>);"
    
  }else if (include.text==F & ! is.null(img.docvar)){
    dx$text <- ""
    text.node <- "<p><b>{name}:</b><br><br><img src='{poster}' width='300' height='450' style='display: block;margin-left: auto;margin-right: auto;></p>"
    
  }else{
    dx$text <- ""
    
    text.node <- "<b>{name}:</b> {value}"
  }
  
  
  if(! is.null(groupvar)){
    group <- quanteda::docvars(corpus, groupvar)
    dx$group <- group
  }else{
    dx$group <- "All"
  } 
  
  
  dt$name[dt$name==""] <- paste0(dt$name[dt$name==""], "Node ", dt$nodeId[dt$name==""])
  
  nodes <- data.frame(nodeId=dt$nodeId, name=dt$name)
  
  # dt$parentId[is.na(dt$parentId)] <- "Root"
  
  dt <- merge(dt, nodes, by.x="parentId", by.y="nodeId", all.x=T)
  
  dt$parentId <- dt$name.y
  dt$nodeId <- dt$name.x
  
  dt <- dt[,c("parentId","nodeId")]
  dt <- merge(dt, dx, by="nodeId", all.x=T)
  dt <- dt[,c("parentId","nodeId","size","group","text","poster")]
  dt <- dt[! is.na(dt$parentId),]
  
  dt$group <- as.character(dt$group)
  dt$group[is.na(dt$group)] <- "All"
  dt$size[is.na(dt$size)] <- 0
  
  gp <- unique(dt$group)
  
  if(length(gp)==1){
    dt$color <- selColors(palette = palette, col.n = nrow(nodes))
  }else{
    pal <- selColors(palette = palette, col.n = length(gp))
    dx <- data.frame(group=gp, color=pal)
    dt <- merge(dt, dx, by="group", all.x=T)
  }
  
  
  # parte 3 - crea el json
  js <- paste0('\n{name: "', dt$nodeId, '", linkWith: ["', dt$parentId,'"], value:', dt$size, ', group: "', dt$group, '", text: "', dt$text ,'", poster:"', dt$poster, '", nodeSettings: {fill: am5.color("', dt$color, '")}}', collapse = ",")
  
  js <- paste0('\nname: "Root",
    value: 0,
    children: [', js, ']')
  
  js <- paste0("{",  js, "}")
  
  data <- htmlwidgets::JS(js)
  

  # forward options using x
  x = list(
    data = data,
    maxRadius = maxRadius,
    tooltipText = text.node,
    BodyStrength = BodyStrength,
    linkWidth = link.width
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'forceClusTree',
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
#' @name forceClusTree-shiny
#'
#' @export
forceClusTreeOutput <- function(outputId, width = '100%', height = '800px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'forceClusTree', width, height, package = 'tenet')
}

#' @rdname forceClusTree-shiny
#' @export
renderforceClusTree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, forceClusTreeOutput, env, quoted = TRUE)
}

