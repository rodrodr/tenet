\name{matchCodes}
\alias{matchCodes}
\title{Cooccurrence of Dictionary Terms in a Corpus}
\description{
Calculates the cooccurrence of dictionary terms in a given corpus.
}
\usage{
matchCodes(corpus, 
              dic,
              level=1,
              remove.self=TRUE,
              quietly=FALSE)
}

\arguments{
\item{corpus}{A quanteda corpus object.}

\item{dic}{Quanteda dictionary employed in the analysis.}

\item{level}{The level of the dictionary to take into account of the aggregation of co-ocurrences. The default is 1 (top level of abstraction).}

\item{remove.self}{Logical. Indicates if the function should remove co-ocurrences of a theme with itself. The default is TRUE.}

\item{quietly}{Logical. Indicates if the function hides the progress bar or not. The default is FALSE.}

}
\details{
The function matchCodes calculates the number of times each pair of dictionary terms co-occur in a corpus.  
}
\value{
A data.frame object containing three variables: term1, term2, and value.
}
\examples{
# Create a corpus object
library(quanteda)
cb <- corpus(spa.inaugural)

# use the dic.pol.es dictionary
dic <- dic.pol.es

# Generates the results
d <- matchCodes(cb, dic)
}