\name{plotVoronoiTree}
\alias{plotVoronoiTree}
\title{Voronoi Tree Diagram}
\usage{
plotVoronoiTree(
                data,
                groups = "groups",
                elements = "elements",
                value = "value",
                height = 500,
                font.size = 11,
                font.size.parent = 25,
                hide.parent.label = FALSE,
                strokeWidth = 2,
                strokeWidthParent = 5,
                strokeColor = "#000",
                type = "polygon",
                cornerCount = 120,
                elementId = "voronoitreediv"
              )
}

\description{
Creates an amCharts Voronoi Tree Diagram for visualiazing hierarchical data.
}

\arguments{
\item{data}{A data.frame with three columns: groups, elements, and value.}

\item{groups}{The name of the variable containing the information about the group, origin or aggregate measure. The default is "group".}

\item{elements}{The name of the variable containing the elements contained in each group. The default is "elements".}

\item{value}{The name of the variable containing the frequency or the value of the elements. The default is "value".}

\item{height}{The height of the html panel in pixels. The default is 500.}

\item{font.size}{The size of the label font in pixels. The default is 11.}

\item{font.size.parent}{The size of the label font in pixels for the groups. The default is 25.}

\item{hide.parent.label}{Logical. Indicates when the labels for the groups should be hidden. The default is FALSE.}

\item{strokeWidth}{The width of the border between elements. The default is 2.}

\item{strokeWidthParent}{The width of the border between groups. The default is 5.}

\item{strokeColor}{The color of the border between elements and groups in hexadecimal. The default is black: "#000".}

\item{type}{The type of graph to be created: "rectangle" or "polygon". The default "polygon".}

\item{cornerCount}{The number of corners or sides when the type is polygon. A triangle would be cornerCount=3, a square: cornerCount=4, a pentagon: cornerCount=5, a hexagon: cornerCount=6, and so on. The default 120 (for a circle).}

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
                        level1=xx$level1), 
                sum, na.rm=T)
    
# Generates a circle voronoi tree diagram
plotVoronoiTree(xx, 
                groups="groups",
                elements="level1",
                value = "frequency")
                    
# Rectangle 
plotVoronoiTree(xx, 
                groups="groups",
                elements="level1",
                value = "frequency",
                type = "rectangle")
    
# Triangle 
plotVoronoiTree(xx, 
                groups="groups",
                elements="level1",
                value = "frequency",
                cornerCount= 3)
}
}
