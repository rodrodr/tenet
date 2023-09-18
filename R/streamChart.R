#
#
# This is the function streamChart.
#
#' @import reshape2
#' @export
streamChart <- function(data, 
                        x=NULL,
                        y=NULL,
                        group = NULL,
                        url.return = FALSE,
                        html.return = FALSE,
                        viewer = TRUE,
                        palette = NULL,
                        height=580,
                        div.name="chartdivstream"){
  
  data <- data[,c(x,y,group)]
  
  data <- reshape2::dcast(data,data[,x]~data[,group], value.var = y)
  
  data[is.na(data)] <- 0
  
  nma <- names(data)[2:ncol(data)]
  nm <- names(data)[2:ncol(data)]
  
  nm <- c("category",paste0("var",1:length(nm)))
  names(data) <-nm
  
  nma <- c("category", nma)
  
  ser <- paste0("createSeries('",
                nm[2:length(nm)], "', '", 
                nma[2:length(nma)],"');", collapse = "\n")
  
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
  
  if(! is.null(palette)){
    pal <- selColors(palette, length(nma)-1)
  }else{
    pal <- selColors(length(nma)-1)
  }

  amcolor <- paste0('am5.color("', pal, '")', collapse = ",\n")
  
  partA <- paste0("<!DOCTYPE html>
    <html>
    <head>
      <meta http-equiv='content-type' content='text/html; charset=UTF-8'>
      <title>tenet - Steam Chart</title>
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
        <script src="https://cdn.amcharts.com/lib/5/xy.js"></script>
        <script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>
        <div id="', div.name, '"></div>
  
      <!-- TODO: Missing CoffeeScript 2 -->
    
      <script type="text/javascript">
    
          var root = am5.Root.new("', div.name,'");
  
          // Set themes
          // https://www.amcharts.com/docs/v5/concepts/themes/
          root.setThemes([
            am5themes_Animated.new(root)
          ]);
  
          var data = [')

  partB <- paste0('];
                  
  // Create wrapper container
var chart = root.container.children.push(am5xy.XYChart.new(root, {
  panX: false,
  panY: false,
  wheelX: "panX",
  wheelY: "zoomX",
  layout: root.horizontalLayout
}));

// Add legend
// https://www.amcharts.com/docs/v5/charts/xy-chart/legend-xy-series/
var legend = chart.children.push(
  am5.Legend.new(root, {
    centerY: am5.p50,
    y: am5.p50,
    layout: root.verticalLayout,
    clickTarget: "none"
  })
);

legend.valueLabels.template.set("forceHidden", true);

// Create axes
// https://www.amcharts.com/docs/v5/charts/xy-chart/axes/
var xAxis = chart.xAxes.push(am5xy.CategoryAxis.new(root, {
  categoryField: "category",
  startLocation: 0.5,
  endLocation: 0.5,
  renderer: am5xy.AxisRendererX.new(root, {
    minGridDistance: 40
  }),
  tooltip: am5.Tooltip.new(root, {})
}));

xAxis.data.setAll(data);

var yAxis = chart.yAxes.push(am5xy.ValueAxis.new(root, {
  renderer: am5xy.AxisRendererY.new(root, {})
}));

// Add series
// https://www.amcharts.com/docs/v5/charts/xy-chart/series/
function createSeries(field, name) {
  var series = chart.series.push(am5xy.SmoothedXLineSeries.new(root, {
    name: name,
    xAxis: xAxis,
    yAxis: yAxis,
    valueField: field,
    valueYField: field + "_hi",
    openValueYField: field + "_low",
    categoryXField: "category",
    tooltip: am5.Tooltip.new(root, {
      pointerOrientation: "horizontal",
      labelText: "[fontSize: 18px]{name}[/] \\n {categoryX}: [bold]{" + field + "}[/]"
    })
  }));
  
  // Do not show tooltip for zero values
  series.get("tooltip").adapters.add("visible", function(visible, target) {
    if (target.dataItem && (target.dataItem.get("value") > 0)) {
      return true;
    }
    return false;
  });

  series.strokes.template.setAll({
    forceHidden: true
  });
  
  series.fills.template.setAll({
    visible: true,
    fillOpacity: 1
  });
  
  chart.get("colors").set("colors", [', amcolor,']);

   // Make stuff animate on load
  // https://www.amcharts.com/docs/v5/concepts/animations/
  series.appear();

  legend.data.push(series);
}

', ser,'

// Prepare data for the river-stacked series
for (var i = 0; i < data.length; i++) {
  var row = data[i];
  var sum = 0;

  // Calculate open and close values
  chart.series.each(function(series) {
    var field = series.get("valueField");
    var val = Number(row[field]);
    row[field + "_low"] = sum;
    row[field + "_hi"] = sum + val;
    sum += val;
  });

  // Adjust values so they are centered
  var offset = sum / 2;
  chart.series.each(function(series) {
    var field = series.get("valueField");
    row[field + "_low"] -= offset;
    row[field + "_hi"] -= offset;
  });
  
  chart.series.each(function(series) {
    var field = series.get("valueField");
    series.data.setAll(data);
  });
}

// Add cursor
chart.set("cursor", am5xy.XYCursor.new(root, {
  behavior: "zoomXY",
  xAxis: xAxis
}));

// Make stuff animate on load
// https://www.amcharts.com/docs/v5/concepts/animations/
chart.appear(1000, 100);
</script>
</body>
</html>')
  
  ht <- paste(partA, vs, partB, collapse = "\n")
  
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
