\name{forceDirectedTree}
\alias{forceDirectedTree}
\title{Force Directed Tree Chart}
\description{
Creates a Force Directed Tree Chart for visualizing hierarchical data.
}
\usage{
forceDirectedTree(json_data, 
                  attraction=-5, 
                  palette="Spectral", 
                  col.n=9, 
                  show.link=TRUE, 
                  height=800,
                  max.radius=5,
                  elementId="chartdiv")
}

\arguments{
\item{json_data}{A json string character as the one produced by the function jsonTree.}

\item{attraction}{The intensity of attraction among nodes. The default is -5.}

\item{palette}{A color palette contained in either RColorBrewer or wesanderson packages. The default is "Spectral".}

\item{col.n}{The number of colors to represent. The default is 9.}

\item{show.link}{Logical. Indicates that links connecting nodes will be made visible. The default is TRUE.}

\item{height}{The height of the html panel in pixels. The default is 800.}

\item{max.radius}{The maximum radius of the nodes in percentage. The default is 5.}

\item{elementId}{Name of the div element employed to contain the graph. It is useful when you use the same type of graph multiple times in the same markdown page, for instance. The default is "chartdiv".}

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
          
# Call the function jsonTree to generate data
jso <- jsonTree(data=dt, 
         groups=c("Characters","Word.Type"),
         elements="Word",
         value="Frequency")

# Create the chart
forceDirectedTree(jso)
}
}
