\name{plotVoronoiTree}
\alias{plotVoronoiTree}
\title{Voronoi Tree Diagram}
\usage{
plotVoronoiTree(
                data,
                value_col="value",
                height = 500,
                font.size = 11,
                font.size.parent = 25,
                hide.parent.label = FALSE,
                strokeWidth = 2,
                strokeWidthParent = 5,
                strokeColor = "#000",
                type = "polygon",
                cornerCount = 120,
                initialDepth=2,
                elementId = "voronoitreediv"
              )
}

\description{
Creates an amCharts Voronoi Tree Diagram for visualiazing hierarchical data.
}

\arguments{
\item{data}{A data.frame with three columns: groups, elements, and value.}

\item{value_col}{The name of the variable containing the frequency or the value to be represented. The default is "value".}

\item{height}{The height of the html panel in pixels. The default is 500.}

\item{font.size}{The size of the label font in pixels. The default is 11.}

\item{font.size.parent}{The size of the label font in pixels for the groups. The default is 25.}

\item{hide.parent.label}{Logical. Indicates when the labels for the groups should be hidden. The default is FALSE.}

\item{strokeWidth}{The width of the border between elements. The default is 2.}

\item{strokeWidthParent}{The width of the border between groups. The default is 5.}

\item{strokeColor}{The color of the border between elements and groups in hexadecimal. The default is black: "#000".}

\item{type}{The type of graph to be created: "rectangle" or "polygon". The default "polygon".}

\item{cornerCount}{The number of corners or sides when the type is polygon. A triangle would be cornerCount=3, a square: cornerCount=4, a pentagon: cornerCount=5, a hexagon: cornerCount=6, and so on. The default 120 (for a circle).}

\item{initialDepth}{Defines the depth of information displayed when the chart is rendered. A initialDepth=1 shows only the top category and the second-level details is informed only when clicked. A initialDepth=2 shows both the first and the second-level categories at once. The first choice makes the graph easier to read, while the second provides a nice visual when second-level categores are not many. The default 2.}

\item{elementId}{Name of the div element employed to contain the graph. It is useful when you use the same type of graph multiple times in the same markdown page, for instance. The default is "voronoitreediv".}
}
\details{
The function generates an interactive Voronoi Tree Diagram for representing hierarchical data. It allows users to explore textual data, codings and themes intuitively.}
\value{
A Voronoi Tree diagram.
}
\examples{
\dontrun{
# Prepares the data
library(quanteda)
cp <- corpus(spa.inaugural)
    
dic <- dic.pol.es
    
xx <- countKeywords(cp, 
                    dic.pol.es, 
                    rel.freq = F, 
                    group.var = "President")
    
xx <- aggregate(list(frequency=xx$frequency), 
                by=list(groups=xx$groups, 
                        level1=xx$level1,
                        level2=xx$level2), 
                sum, na.rm=T)
    
# Generates a circle voronoi tree diagram
plotVoronoiTree(xx, 
                value_col = "frequency")
                    
# Rectangle 
plotVoronoiTree(xx, 
                value_col = "frequency",
                type = "rectangle")
    
# Triangle 
plotVoronoiTree(xx, 
                value_col = "frequency",
                cornerCount= 3)
}
}
