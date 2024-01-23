\name{forceDirectedTree}
\alias{forceDirectedTree}
\title{Force Directed Tree Chart}
\description{
Creates a Force Directed Tree Chart for visualizing hierarchical data.
}
\usage{
forceDirectedTree(data,
                  value_col="value",
                  attraction=-5, 
                  palette=c("#DD8D29","#E2D200","#46ACC8","#E58601","#B40F20"), 
                  col.n=9, 
                  show.link=TRUE, 
                  height=800,
                  width="100\%",
                  max.radius=5,
                  elementId="chartdiv",
                  tooltip.text="{name}: {value}")
}

\arguments{
\item{data}{An hierarchical data.frame to be converted in a json tree.}

\item{value_col}{The name of the variable containing the frequency or the value to be represented. The default is "value".}

\item{attraction}{The intensity of attraction among nodes. The default is -5.}

\item{palette}{A color palette to be used to represent nodes. The default is NULL (the default would be "Dark2" from RColorBrewer).}

\item{col.n}{The number of colors to represent. The default is 9.}

\item{show.link}{Logical. Indicates that links connecting nodes will be made visible. The default is TRUE.}

\item{height}{The height of the html panel in pixels. The default is 800.}

\item{width}{The width of the html panel in pixels or percent. The default is 100\%.}

\item{max.radius}{The maximum radius of the nodes in percentage. The default is 5.}

\item{elementId}{Name of the div element employed to contain the graph. It is useful when you use the same type of graph multiple times in the same markdown page, for instance. The default is "chartdiv".}

\item{tooltip.text}{This option allows users to customize the text or html code that will be displayed on the tooltip of the chart. The default is "{name}: {value}".}
}
\details{
The function generates a Force Directed Tree Graph. It allows users to explore hierarchical data up to three levels.}
\value{
A Force Directed Tree chart or the html source code generated.
}
\examples{
\dontrun{
# Create a test dataset
dt <- data.frame(
          Characters=c("Joao Grilo","Chico"), 
          Word.Type= c("Verb","Noun"), 
          Word=c("sing","harmonica"), 
          Frequency=c(2,3))
          
# Create the chart
forceDirectedTree(dt, value_col="Frequency")
}
}
