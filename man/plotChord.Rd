\name{plotChord}
\alias{plotChord}
\title{Co-ocurrence Chord Diagram for Dictionary Codes and Themes}
\description{
Generates Chord Diagram for the Co-Ocurrence of Dictionary Themes in a Corpus. 
}
\usage{
plotChord(data, 
          from="from", 
          to="to", 
          value="value", 
          font.size=12, 
          node.width=5, 
          opacity=0.05, 
          radius.percent=70, 
          height=600, 
          elementId="chordtheme")
}

\arguments{
\item{data}{A data.frame with three columns: origin, destination, and value.}

\item{from}{The name of the variable containing the information about the group, origin or aggregate measure. The default is "from".}

\item{to}{The name of the variable containing the information about the detail category, destination or attribute. The default is "to".}

\item{value}{The name of the variable containing the frequency or the value of the relationship between a given origin and destination or group and attribute. The default is "value".}

\item{font.size}{The size of the font in pixels. The default is 12.}

\item{node.width}{The width (thickness) of the node bar. The default is 5.}

\item{opacity}{The opacity of the links when not hovered. The default is 0.05 (5 percent opacity).}

\item{radius.percent}{Percentage of the height occupied by the radius of the diagram. The default is 70.}

\item{height}{The height of the html panel in pixels. The default is 580.}

\item{elementId}{Name of the div element employed to contain the graph. It is useful when you use the same type of graph multiple times in the same markdown page, for instance. The default is "chordtheme".}
}
\details{
The function generates an interactive Chord Diagram. It allows users to perform thematic analysis and other categorical hierarchical data analysis intuitively.}
\value{
A chart representing the similarity of texts or the html source code generated.
}
\examples{
\dontrun{
# Retrieve a corpus of text 
cp <- quanteda::corpus(spa.inaugural)

# Loads the dictionary for Spanish Speeches
dic <- dic.pol.es

# Generate the data to be employed
d <- matchCodes(cp, dic)

# Create the graph for the inaugural discourses
plotChord(d, from="term1", to="term2", value="value")
}
}
