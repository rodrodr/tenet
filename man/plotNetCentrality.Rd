\name{plotNetCentrality}
\alias{plotNetCentrality}
\title{Network Centrality Plot}
\description{
Creates a bar plot with key network centrality measures.
}
\usage{
plotNetCentrality(g, 
                  title="Network Centrality Measures", 
                  subtitle="Ranking of nodes according to centrality.",
                  caption="Source: Own elaboration.",
                  palette=c("#1B9E77", 
                            "#D95F02", 
                            "#7570B3", 
                            "#E7298A", 
                            "#66A61E", 
                            "#E6AB02", 
                            "#A6761D", 
                            "#666666"), 
                  methods=c("degree",
                            "authority",
                            "page_rank",
                            "eigenvector",
                            "betweenness",
                            "closeness",
                            "hub"),
                  topn=20,
                  ties.method="min")
}

\arguments{

\item{g}{An igraph object.}

\item{title}{A character string. The title of the chart. The default is "Network Centrality Measures".}

\item{subtitle}{A character string. The subtitle of the chart. The default is "Ranking of nodes according to centrality.".}

\item{caption}{A character string. The caption of the chart. The default is "Source: Own elaboration.".}

\item{palette}{A character vector. The color palette to be used in the chart.}

\item{methods}{A character vector. The network centrality measures to be used in the chart. The default is c("degree", "authority", "page_rank", "eigenvector", "betweenness", "closeness", "hub").}

\item{topn}{An integer. The number of top nodes to be displayed in the chart. The default is 20.}

\item{ties.method}{A character string. The method to break ties in the ranking. The default is "min".}

}
\details{
The function corNet generates a network graph (sociogram) for the correlation among words in a corpus. The default centrality measures are: degree, authority, page rank, eigenvector, betweenness, closeness and hub.
}
\value{
A ggplot2 chart.
}
\examples{
\dontrun{

# Create a random network graph
library(igraph)
g <- sample_gnp(100, 1/100)

# Plot the network centrality measures
plotNetCentrality(g)

}
}