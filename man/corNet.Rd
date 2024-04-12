\name{corNet}
\alias{corNet}
\title{Sociogram of a Correlation Matrix}
\description{
Generates a network chart of correlations among words in a corpus.
}
\usage{
corNet(cor.list, 
       link.col=c("red","steelblue1"),
       link.alpha=0.7,
       link.curvature=0.25,
       node.col="purple",
       family="sans",
       layout="fr")
}

\arguments{
\item{cor.list}{A list containing a edge list and details on vectors produces by the function corTerms.}
\item{link.col}{The color of the links/edges among words. The default are red (for negative correlations) and steelblue1 (for positive ones).}
\item{link.alpha}{The opacity of link colors. The default is 0.3.}
\item{link.curvature}{The curvature of the links/edges in the graph. The default is 0.25.}
\item{node.col}{Indicates the degree of correlation that will be used to filter the values returned. The default is "purple".}
\item{family}{Font family to be used in labeling values. The default is "sans".}
\item{layout}{The network layout of the graph. The possible options are: "circle","sphere","nicely","fr","gem","kk","mds","dh","lgl","sugiyama". The default is "fr".}
}
\details{
The function corNet generates a network graph (sociogram) for the correlation among words in a corpus.  
}
\value{
A ggplot2 chart.
}
\examples{
\dontrun{
# Create a corpus object
cb <- bra.inaugural

# Generates a list of correlations
ll <- corTerms(cb, 
                lang = "pt", 
                min.freq = 50, 
                n.terms = 50, 
                remove.wordlist = c("é",
                                "ser",
                                "fazer",
                                "cada",
                                "neste"))

# Plots the sociogram
corNet(ll)
}
}

