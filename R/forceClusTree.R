#
#
# This is the function forceClusTree.
#
#' @import stringi
#' @import quanteda
#' @import quanteda.textstats
#' @export
forceClusTree <- function(corpus, 
                           remove_punct=TRUE,
                           lang = "es",
                           url.return = FALSE,
                           html.return = FALSE,
                           viewer = TRUE,
                           custom.color = NULL,
                           palette = "BottleRocket2",
                           custom.pal = NULL,
                           groupvar = NULL,
                           height=580,
                           link.col="#734421",
                           link.width=2,
                           minRadius=0,
                           maxRadius=100,
                           BodyStrength=-10,
                           include.text=FALSE,
                           img.docvar=NULL,
                           clust.method="euclidean"){
  
  
  # parte 1 - cluster
  tk <- quanteda::tokens(corpus, remove_punct = remove_punct) |> 
    quanteda::tokens_remove(stopwords(lang)) |> 
    quanteda::dfm() |> 
    quanteda::dfm_trim(min_termfreq = 5, min_docfreq = 3)
  
  tkd <- quanteda::dfm_weight(tk, scheme = "prop") %>%
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
      dx$text[i] <- paste(stri_wrap(dx$text[i], 60, 0.0, ), collapse = "<br>")
    }
    
    text.node <- paste0('series.nodes.template.set(', '"tooltipHTML",','"',"<p><b>{name}:</b><br><br>{text}<br><br><img src='{poster}' width='300' height='450' style='display: block;margin-left: auto;margin-right: auto;'",'></p>");')
  }else if (include.text==T & is.null(img.docvar)){
    
    dx$text <- as.character(corpus) 
    dx$text <- gsub('"',"'", dx$text, fixed = T)
    
    for (i in 1:nrow(dx)){
      dx$text[i] <- paste(stringi::stri_wrap(dx$text[i], 60, 0.0, ), collapse = "<br>")
    }
    
    text.node <- paste0('series.nodes.template.set("tooltipHTML","<p><b>{name}:</b><br><br>{text}</p>);"')
    
  }else if (include.text==F & ! is.null(img.docvar)){
    dx$text <- ""
    text.node <- paste0('series.nodes.template.set(', '"tooltipHTML",','"',"<p><b>{name}:</b><br><br><img src='{poster}' width='300' height='450' style='display: block;margin-left: auto;margin-right: auto;'",'></p>");')
    
  }else{
    dx$text <- ""
    
    text.node <- 'series.nodes.template.set("tooltipHTML","<b>{name}:</b> {value}");'
    
    
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
    if(! is.null(custom.color)){
      dt$color <- custom.color
    }else{
      dt$color <- selColors(palette = palette, col.n = nrow(nodes), custom.pal = custom.pal)
    }
  }else{
    pal <- selColors(palette, length(gp), custom.pal = custom.pal)
    dx <- data.frame(group=gp, color=pal)
    dt <- merge(dt, dx, by="group", all.x=T)
  }
  
  
  # parte 3 - crea el json
  js <- paste0('\n{name: "', dt$nodeId, '", linkWith: ["', dt$parentId,'"], value:', dt$size, ', group: "', dt$group, '", text: "', dt$text ,'", poster:"', dt$poster, '", nodeSettings: {fill: am5.color("', dt$color, '")}}', collapse = ",")
  
  js <- paste0('\nname: "Root",
    value: 0,
    children: [', js, ']')
  
  
  amcolor <- paste0(rep(paste0('am5.color("', link.col, '")'), nrow(dt)), collapse = ",\n")
  
  # parte 4 - crea el html
  
  partA <- paste0('<!DOCTYPE html>
    <html>
    <head>
      <meta http-equiv="content-type" content="text/html; charset=UTF-8">
      <title>Force Directed Dendrogram</title>
      <meta http-equiv="content-type" content="text/html; charset=UTF-8">
      <meta name="robots" content="noindex, nofollow">
      <meta name="googlebot" content="noindex, nofollow">
      <meta name="viewport" content="width=device-width, initial-scale=1">
    
  
  <style id="compiled-css" type="text/css">
          body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
  }
  
  #chartdivclus {
    width: 100%;
    height: ',height, 'px;
  }
  </style>
    
    </head>
    <body>
        <script src="https://cdn.amcharts.com/lib/5/index.js"></script>
        <script src="https://cdn.amcharts.com/lib/5/hierarchy.js"></script>
        <script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>
        <div id="chartdivclus"></div>
  
    
      <!-- TODO: Missing CoffeeScript 2 -->
    
      <script type="text/javascript">
    
          var root = am5.Root.new("chartdivclus");
  
          // Set themes
          // https://www.amcharts.com/docs/v5/concepts/themes/
          root.setThemes([
            am5themes_Animated.new(root)
          ]);
  
          var data = {')
  
  
  partB <- paste0('};
  
  // Create wrapper container
        var container = root.container.children.push(
          am5.Container.new(root, {
            width: am5.percent(100),
            height: am5.percent(100),
            layout: root.verticalLayout
          })
        );
        
        // Create series
        // https://www.amcharts.com/docs/v5/charts/hierarchy/#Adding
        var series = container.children.push(
          am5hierarchy.ForceDirected.new(root, {
            singleBranchOnly: false,
            downDepth: 1,
            topDepth: 1,
            maxRadius: ', maxRadius,',
            minRadius: ', minRadius,',
            valueField: "value",
            categoryField: "name",
            childDataField: "children",
            colorField: "color",
            idField: "name",
            linkWithStrength: 1,
            linkWithField: "linkWith",
            manyBodyStrength: ',BodyStrength,',
            centerStrength: 0.5
          })
        );
        
        series.circles.template.setAll({
            templateField: "nodeSettings"
        });', text.node,'
  
        series.get("colors").set("colors", [', amcolor,']);

        series.data.setAll([data]);
        series.set("selectedDataItem", series.dataItems[0]);
        
        series.links.template.setAll({
        colorMode: "solid",
        fill: am5.color("#000000"),
        strokeWidth: ',link.width, ',
        strokeOpacity: 1,
      });
  
  
        // Make stuff animate on load
        series.appear(1000, 100);
  
      </script>
  
  </body>
    </html>')
  
  ht <- paste(partA, js, partB, collapse = "\n")
  
  tp <- tempfile(pattern = "temp", fileext = ".html")
  
  write(ht, tp)
  
  if(html.return==F & url.return==F){
    if(viewer==T){
      rstudioapi::viewer(tp)
    }else{
      browseURL(tp)
    }
  }else if(html.return==F & url.return==T){
    return(tp)
  }else{
    return(ht)
  }
  
}

