\name{plotSankey}
\alias{plotSankey}
\title{Sankey Diagram}
\description{
Creates a Sankey Diagram for two-level data.
}
\usage{
plotSankey(data, 
           from="from", 
           to="to", 
           value="value",
           font.size=12, 
           opacity=0.05, 
           paddingRight=150, 
           elementId="sankeydiv", 
           height=600)
}

\arguments{
\item{data}{A data.frame with three columns: origin, destination, and value.}

\item{from}{The name of the variable containing the information about the group, origin or aggregate measure. The default is "from".}

\item{to}{The name of the variable containing the information about the detail category, destination or attribute. The default is "to".}

\item{value}{The name of the variable containing the frequency or the value of the relationship between a given origin and destination or group and attribute. The default is "value".}

\item{font.size}{The size of the label font in pixels. The default is 12.}

\item{opacity}{The opacity of the links when not hovered. The default is 0.05 (5 percent opacity).}

\item{paddingRight}{The size margin on the right side of the chart for the labels. The default is 150 pixels.}

\item{elementId}{Name of the div element employed to contain the graph. It is useful when you use the same type of graph multiple times in the same markdown page, for instance. The default is "sankeydiv".}

\item{height}{The height of the html panel in pixels. The default is 600.}
}
\details{
The function generates an interactive Sankey Diagram for representing hierarchical data. It allows users to explore textual data, codings and themes intuitively.}
\value{
A Sankey diagram or the html source code generated.
}
\examples{
\dontrun{
# Create a test dataset
cp <- corpus(spa.inaugural)

dic <- dic.pol.es

xx <- countKeywords(cp, dic.pol.es, rel.freq = F, group.var = "President")

xx <- aggregate(list(frequency=xx$frequency), 
                by=list(groups=xx$groups, 
                        level1=xx$level1,
                        level2=xx$level2), 
                sum, na.rm=T)
          
# Create the chart
plotSankey(data=xx,
           from="groups",
           to="level1",
           value="frequency")
}
}
