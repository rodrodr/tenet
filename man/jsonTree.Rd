\name{jsonTree}
\alias{jsonTree}
\title{Two-level Hierarchical Data to JSON}
\description{
Translates a data.frame containing a two-level hierarchy into a json format able to be use in the amCharts Force Directed Network chart.
}
\usage{
jsonTree(data, 
         groups, 
         elements, 
         value)
}

\arguments{
\item{data}{A data.frame object with at least four variables: level 1 groups, level 2 groups, elements forming the level 2 groups, numeric value of the elements.}
\item{groups}{Names of the two grouping variables in the data.frame.}
\item{elements}{Name of the variable containing the elements.}
\item{value}{Name of the numeric variable with the values.}
}
\details{
The function jsonTree returns a json string formatted to represent a two-level hierarchical network compatible to the amCharts Collapsible Force Directed Tree algorithm. This function will generate the data for the forceDirectedTree function in the package.

The data.frame should have at least four variables: two groups, one list of elements, and another of values. For instance, it can be: data.frame(Characters=c("Joao Grilo","Chico"), Word.Type= c("Verb","Noun"), Word=c("sing","harmonica"), Frequency=c(2,3)). In this case, "Characters" represents the level 1 group, "Word.Type" rests as the level 2 group, "Word" signals the elements, and "Frequency" indicates the values. 
}
\value{
The result is a character string formatted as a json structure of a two-level tree hierarchy.
}
\examples{
\dontrun{
# Create a test dataset
dt <- data.frame(
          Characters=c("Joao Grilo","Chico"), 
          Word.Type= c("Verb","Noun"), 
          Word=c("sing","harmonica"), 
          Frequency=c(2,3))
          
# Call the function
jsonTree(data=dt, 
         groups=c("Characters","Word.Type"),
         elements="Word",
         value="Frequency")
}
}
