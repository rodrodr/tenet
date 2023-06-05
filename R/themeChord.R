#
#
# This is the function themeChord.
#
#' @import reshape2
#' @import quanteda
#' @import stringi
#' @export
themeChord <- function(corpus, dic, level=1, remove.self=TRUE, font.size=12, node.width=5, opacity=0.05, div.name="chordtheme", height=600, radius.percent=70, viewer=TRUE, html.return=FALSE, url.return=FALSE){
  
  
  fc <- quanteda::fcm(quanteda::tokens(corpus))
  ff <- quanteda::fcm_select(fc, pattern = unlist(dic), valuetype = "regex")
  
  dm <- quanteda::convert(ff, "data.frame")
  dm <- reshape2::melt(dm, id.vars = "doc_id")
  dm <- dm[dm$value>0,]
  dm$term1 <- NA
  dm$term2 <- NA
  
  
  if(level>1){
    dic <- unlist(dic, recursive = F)
    dic <- quanteda::dictionary(dic)
    nm <- names(dic)
    nma <- stringi::stri_split_fixed(nm, ".", 2, simplify = T)[,2]
  }else{
    nm <- names(dic)
    nma <- nm  
  }
  
  
  for(i in 1:length(nm)){
    
    ky <- unlist(dic[nm[i]], use.names = F)
    
    for(k in 1:length(ky)){
      
      dm$term1[which(stringi::stri_detect_regex(dm$doc_id, pattern = ky[k], case_insensitive=T)==TRUE)] <- nma[i]
      
      dm$term2[which(stringi::stri_detect_regex(dm$variable, pattern = ky[k], case_insensitive=T)==TRUE)] <- nma[i]
      
      
    }  
    
  }
  
  dm <- aggregate(list(value=dm$value), by=list(term1=dm$term1, term2=dm$term2), sum, na.rm=T)
  
  
  dm <- aggregate(value~term1 + term2, data=transform(dm, term1 = pmin(term1, term2),term2 = pmax(term1, term2)), sum, na.rm = TRUE)
  
  dm <- dm[order(dm$term1, dm$term2),]
  
  if(remove.self==TRUE){
    dm <- dm[dm$term1!=dm$term2,]
  }
  
  dm$value <- round(dm$value/2)
  
  data <- paste0('{ "from": "', dm$term1, '", "to": "', dm$term2,'", "value":', dm$value, '}', collapse = ",\n")
  
  
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
  
  #', div.name, ' {
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


// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);

// Create series
// https://www.amcharts.com/docs/v5/charts/flow-charts/
var series = root.container.children.push(am5flow.ChordDirected.new(root, {
  startAngle: 80,
  padAngle: 2,
  linkHeadRadius: undefined,
  sourceIdField: "from",
  targetIdField: "to",
  valueField: "value",
  nodeWidth: ',node.width,',
  radius: am5.percent(',radius.percent,')
}));

series.nodes.labels.template.setAll({
  textType: "radial",
  centerX: 0,
  fontSize: ',font.size,'
});


series.links.template.set("fillStyle", "source");

var linkTemplate = series.links.template;
linkTemplate.setAll({
	strokeOpacity: 0,
  fillOpacity: ',opacity,'
});

var hoverState = linkTemplate.states.create("hover", {
	fillOpacity: 1,
  strokeOpacity: 0});


// Set data
// https://www.amcharts.com/docs/v5/charts/flow-charts/#Setting_data
series.data.setAll([')
  
  
  partB <- paste0(']);


// Make stuff animate on load
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
