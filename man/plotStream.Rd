\name{plotStream}
\alias{plotStream}
\title{Stream Chart}
\description{
Generates an Interactive Stream Chart. 
}
\usage{
plotStream(data, 
           x=NULL,
           y=NULL,
           group = NULL,
           palette = c("#DD8D29","#E2D200","#46ACC8","#E58601","#B40F20"),
           height=580,
           elementId="chartdivstream")
}

\arguments{
\item{data}{A data.frame object containing data on the categories, the timeline or sequence of events and the quantity.}

\item{x}{The name of the variable in data containing the information relative to the timeline or the sequence of events.}

\item{y}{The name of the variable containing the quantities to be represented.}

\item{group}{The name of the variable indicating groups or aggregated categories.}

\item{palette}{The color palette included in either RColorBrewer or wesanderson palettes. The default is "FantasticFox1".}

\item{height}{The height of the html panel in pixels. The default is 580.}

\item{elementId}{Name of the div element employed to contain the graph. It is useful when you use the same type of graph multiple times in the same markdown page, for instance. The default is "chartdivstream".}

}
\details{
The function generates an interactive Stream Chart. It represents data along a central baseline. It is similar to an area chart, but splits the values using a central line of reference.}
\value{
A chart representing the evolution in time or sequence of a variable or the html source code generated.
}
\examples{
\dontrun{
# Select the most salient representatives for 
# the Vox party
ag <- spa.sessions[
        spa.sessions$rep.name\%in\%
          c("Abascal Conde, Santiago",
            "Espinosa de los Monteros de Simón, Iván",
            "Olona Choclán, Macarena",                
            "Ortega Smith-Molina, Francisco Javier"),]

# Create a variable of month for smoothing the data
ag$month <- substr(ag$session.date,3,7)

# Aggregate words by representative and month
ag <- aggregate(
    list(words=ag$speech.tokens), 
      by=list(
        month=ag$month, 
        rep=ag$rep.name, 
        party=ag$rep.party), 
      sum, 
      na.rm=T)

# Order the data by month
ag <- ag[order(ag$month),]

# Create the chart
plotStream(ag, 
           x="month", 
           y="words", 
           group = "rep")
}
}
