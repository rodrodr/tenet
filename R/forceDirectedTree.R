#
#
# forceDirectedTree
# 
#' @export
forceDirectedTree <- function(json_data, desc="units", attraction= -5, collapsed=FALSE, palette="Spectral", col.n=9, show.link=TRUE, url.return=FALSE, html.return=FALSE, viewer=TRUE, height=800){
  
  co <- selColors(palette = palette, col.n = col.n)
  
  co <- paste0('am4core.color("', co,'")',collapse = ",\n")
  
  co <- paste('\nnetworkSeries.colors.list = [\n', co, '\n];\n')
  
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
  
  #chartdiv {
    width: 100%;
    max-width: 100%;
    height:',height,'px;
  }
    </style>
  
  </head>
  <body>
      <script src="https://www.amcharts.com/lib/4/core.js"></script>
  <script src="https://www.amcharts.com/lib/4/charts.js"></script>
  <script src="https://www.amcharts.com/lib/4/plugins/forceDirected.js"></script>
  <script src="https://www.amcharts.com/lib/4/themes/animated.js"></script>
  <div id="chartdiv"></div>
  
    <!-- TODO: Missing CoffeeScript 2 -->
  
    <script type="text/javascript">//<![CDATA[
  
      
  // Themes begin
  am4core.useTheme(am4themes_animated);
  // Themes end
  
  var chart = am4core.create("chartdiv", am4plugins_forceDirected.ForceDirectedTree);
  
  var networkSeries = chart.series.push(new am4plugins_forceDirected.ForceDirectedSeries())', co ,'
  
  networkSeries.data = [')
  
  
  partB <- paste0('];
  
  networkSeries.dataFields.linkWith = "linkWith";
  networkSeries.dataFields.name = "name";
  networkSeries.dataFields.id = "name";
  networkSeries.dataFields.collapsed = "true";
  networkSeries.dataFields.value = "value";
  networkSeries.dataFields.children = "children";
  networkSeries.links.template.distance = 1;
  networkSeries.nodes.template.tooltipText = "{name}: [font-size: 30px;]{value} [font-size: 12px;] ', desc ,'.";
  networkSeries.nodes.template.fillOpacity = 1;
  networkSeries.nodes.template.outerCircle.scale = 1;
  
  networkSeries.nodes.template.label.text = "{name}"
  networkSeries.fontSize = 14 ;
  networkSeries.nodes.template.label.hideOversized = true;
  networkSeries.nodes.template.label.truncate = true;
  networkSeries.minRadius = am4core.percent(0.35);
  networkSeries.maxRadius = am4core.percent(5);
  networkSeries.manyBodyStrength =', attraction, ';
  networkSeries.links.template.strokeOpacity = ',sl,';', colla, '
  
    //]]></script>
  
    <script>
      // tell the embed parent frame the height of the content
      if (window.parent && window.parent.parent){
        window.parent.parent.postMessage(["resultsFrame", {
          height: document.body.getBoundingClientRect().height,
          slug: "56rk84ve"
        }], "*")
      }
  
      // always overwrite window.name, in case users try to set it manually
      window.name = "result"
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
