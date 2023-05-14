\name{pcaScatter}
\alias{pcaScatter}
\title{Visualization of Principal Component and Correspondence Analysis}
\description{
Performs a Principal Component or Correspondence Analysis
on a text corpus and plots the results in an interactive scatter plot.
}
\usage{
pcaScatter(corpus, 
           lang="es",
           min.freq = 100,
           n.clusters = 4,
           interactive = TRUE,
           type = "pca",
           title = "Title",
           caption = "Source: Own elaboration.",
           alpha = 0.5,
           palette = "EdwardHopper",
           custom.pal = NULL)
}

\arguments{
\item{corpus}{A quanteda corpus containing texts.}
\item{lang}{The language for removing stopwords. The default is Spanish: "es".}
\item{min.freq}{The minimum frequency to be included in the analysis. The default is 100.}
\item{n.clusters}{The number of clusters to divide the results into groups. The default is 4.}
\item{interactive}{Logical. Indicates whether the chart will be interactive or a ggplot2 object will be returned. The default is TRUE.}
\item{type}{Indicates whether the analysis will be a PCA (type="pca") or a Correspondence Analysis (type="ca"). The default is "pca".}
\item{title}{The title of the graph. The default is "Title".}
\item{caption}{The caption of the graph. The default is "Source: Own elaboration.".}
\item{alpha}{The opacity of the colors. The default is 0.5 (50 percent opaque).}
\item{palette}{One of the palettes included in the listPalettes function of tenet. The default is "EdwardHopper".}
\item{custom.pal}{The list of colors used for creating a customized color palette. The default is NULL.}
}
\details{
The function pcaScatter allows users to perform two dimension reduction analysis on text data: Principal Component Analysis and Correspondence Analysis. It also applies a hierarchical cluster algorithm to the results to separate terms into groups based on their similarity. 
}
\value{
The results are either an interactive graph or a ggplot2 object to be further edited by the user.
}
\examples{
# Create a corpus object
library(quanteda)
cp <- corpus(spa.inaugural)

# Generates a PCA using pcaScatter
pcaScatter(cp, 
           title = "Disc. Inauguración (1979-2019)", 
           palette="GeneDavis", 
           min.freq = 10)
           
# Now, performs a Correspondence Analysis
pcaScatter(cp, 
           type="ca",
           title = "Disc. Inauguración (1979-2019)", 
           palette="GeneDavis", 
           min.freq = 10)
}
