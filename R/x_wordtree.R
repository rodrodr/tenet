#
#
# Function x_wordtree
# Creates a wordtree for visualizing keywords in context
#' @import stringi
# @export
x_wordtree <- function(text,
                     targetWord,
                     direction="double",
                     window=5,
                     height=900){
  
  window <- window
  
  text <- gsub(pattern = "[^a-zA-Z ]","",x=text)
  
  text <- gsub(pattern = "http[a-zA-Z]+","",x=text,perl = T)
  
  S <- "( ?\\w+ ?)"
  
  if(direction=="double"){
    RE <- paste0("(\\S+\\s+|^)",
                 paste(rep(S,window),collapse=""),
                 targetWord,
                 paste(rep(S,window),collapse=""),
                 "(\\s+\\S+|$)")
  } else if(direction=="suffix"){
    RE <- paste0(
      targetWord,paste(rep(S,window),collapse=""),
      "(\\s+\\S+|$)")
    
  } else if(direction=="prefix"){
    RE <- paste0("(\\S+\\s+|^)",paste(rep(S,window),collapse=""),targetWord)
    
  }
  
  x <- stringi::stri_extract(text,regex=RE)
  x <- x[!is.na(x)]
  x <- paste(x,collapse="'],['")
  x <- paste0("['",x,"']")
  
  top= "<html>
  <head>
  <script type=\"text/javascript\" src=\"https://www.gstatic.com/charts/loader.js\"></script>
  <script type=\"text/javascript\">
  google.charts.load('current', {packages:['wordtree']});
  google.charts.setOnLoadCallback(drawChart);

  function drawChart() {
  var data = google.visualization.arrayToDataTable(
  [ ['Phrases'],"
  
  bottom <- paste0("]
  );

                   var options = {
                   wordtree: {
                   format: 'implicit',
                   word: '",targetWord,"',
                   type: '",direction,"'
                   }
                   };

                   var chart = new google.visualization.WordTree(document.getElementById('wordtree_basic'));
                   chart.draw(data, options);
  }
                   </script>
                   </head>
                   <body>
                   <div id=\"wordtree_basic\" style=\"width:100%; height:", height, "px;\"></div>
                   </body>
                   </html>")
  
  paste0(top,x,bottom, collapse = "\n")
}

