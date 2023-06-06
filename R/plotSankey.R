#
#
# This is the function plotSankey.
#
#' @export
plotSankey <- function(data, from="from", to="to", value="value",
                       font.size=12, opacity=0.05, paddingRight=150, 
                       div.name="sankeydiv", height=600, viewer=TRUE, 
                       html.return=FALSE, url.return=FALSE){
  
  
  data <- paste0('{ from: "', data[,from], '", to: "',data[,to],'", value: ', data[,value], '}', collapse = ',\n')
  
  
  partA <- paste0("<!DOCTYPE html>
    <html>
    <head>
      <meta http-equiv='content-type' content='text/html; charset=UTF-8'>
      <title>Force Directed Dendrogram</title>
      <meta http-equiv='content-type' content='text/html; charset=UTF-8'>
      <meta name='robots' content='noindex, nofollow'>
      <meta name='googlebot' content='noindex, nofollow'>
      <meta name='viewport' content='width=device-width, initial-scale=1'>
    
  
  <style id='compiled-css' type='text/css'>
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol';
  }
  
  #", div.name, ' {
    width: 100%;
    height: ',height, 'px;
  }
  </style>
  
      </head>
    <body>
        <script src="https://cdn.amcharts.com/lib/5/index.js"></script>
        <script src="https://cdn.amcharts.com/lib/5/flow.js"></script>
        <script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>
        <div id="', div.name, '"></div>
  
      <script type="text/javascript">
  
  var root = am5.Root.new("', div.name, '");

root.setThemes([
  am5themes_Animated.new(root)
]);

var series = root.container.children.push(am5flow.Sankey.new(root, {
  sourceIdField: "from",
  targetIdField: "to",
  valueField: "value",
  paddingRight: ',paddingRight,'
}));

series.nodes.get("colors").set("step", 2);

var linkTemplate = series.links.template;
linkTemplate.setAll({
	strokeOpacity: 0,
  fillOpacity: ', opacity ,'
});

var hoverState = linkTemplate.states.create("hover", {
	fillOpacity: 1,
  strokeOpacity: 0});

series.nodes.labels.template.setAll({
  textType: "radial",
  centerX: 0,
  fontSize: ',font.size,'
});

series.data.setAll([')
  
  
  partB <- paste0(']);


series.appear(1000, 100);
</script>
  
  </body>
  </html>')
  
  ht <- paste(partA, data, partB, collapse = "\n")
  
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

