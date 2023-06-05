#
#
# forceDirectedTree
# 
#' @export
forceDirectedTree <- function(json_data, attraction= -5, collapsed=FALSE, palette="Spectral", col.n=9, show.link=TRUE, url.return=FALSE, html.return=FALSE, viewer=TRUE, height=800, max.radius=5, div.name="chartdiv"){
  
  max.radius <- as.character(max.radius)
  
  co <- selColors(palette = palette, col.n = col.n)
  
  co <- paste0('am5.color("', co,'")',collapse = ",\n")
  
  colla <- "" 
  sl <- 1

  if(show.link==F){
    sl <- 0
  }
  
  if(collapsed==T){
    
    colla <-   "
    
    // Start collapsed
    networkSeries.maxLevels = 1;
    
    // Expand single level only
    networkSeries.nodes.template.expandAll = false;"
    
  }
  
  partA <- paste0('<!DOCTYPE html>
  <html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <title>Collapsible force-directed tree</title>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <meta name="robots" content="noindex, nofollow">
    <meta name="googlebot" content="noindex, nofollow">
    <meta name="viewport" content="width=device-width, initial-scale=1">
  
  
    <style id="compiled-css" type="text/css">
        body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
  }
  
  #',div.name,' {
    width: 100%;
    max-width: 100%;
    height:',height,'px;
  }
    </style>
  
  </head>
  <body>
<script src="https://cdn.amcharts.com/lib/5/index.js"></script>
<script src="https://cdn.amcharts.com/lib/5/hierarchy.js"></script>
<script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>  

<div id="',div.name,'"></div>
  
    <!-- TODO: Missing CoffeeScript 2 -->
  
    <script type="text/javascript">//<![CDATA[
  
      
var root = am5.Root.new("chartdiv");

// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);

var data = {
  value: 0,
  children: [')
  
  
  
  partB <- paste0(']};
                
// Create wrapper container
var container = root.container.children.push(am5.Container.new(root, {
  width: am5.percent(100),
  height: am5.percent(100),
  layout: root.verticalLayout
}));

var series = container.children.push(am5hierarchy.ForceDirected.new(root, {
  singleBranchOnly: false,
  downDepth: 2,
  topDepth: 1,
  initialDepth: 1,
  valueField: "value",
  categoryField: "name",
  childDataField: "children",
  idField: "name",
  linkWithField: "linkWith",
  manyBodyStrength: ', attraction, ',
  minRadius: am5.percent(0.35),
  maxRadius: am5.percent(', max.radius ,'),
  centerStrength: 0.8
}));

series.get("colors").set("colors",[
', co ,'
]);

series.links.template.set("strength", 0.9);

series.links.template.set("strokeOpacity", ',sl,');

series.data.setAll([data]);

series.set("selectedDataItem", series.dataItems[0]);

// Make stuff animate on load
series.appear(1000, 100);

</script>
</body>
</html>')
  
  
  
  ht <- paste(partA, json_data, partB, collapse = "\n")
  
  tp <- tempfile(pattern = "temp", fileext = ".html")
  
  write(ht, tp)
  
  
  if(html.return==F & url.return==F){
    if(viewer==T){
      rstudioapi::viewer(tp)
    }else{
      utils::browseURL(tp)
    }
  }else if(html.return==F & url.return==T){
    return(tp)
  }else{
    return(ht)
  }
  
  
}  
