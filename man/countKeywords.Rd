\name{countKeywords}
\alias{countKeywords}
\title{Frequency of Dictionary Terms in a Corpus}
\description{
Calculates the absolute or relative frequency of dictionary terms in a given corpus.
}
\usage{
countKeywords(corpus, 
              dic, 
              group.var=NULL, 
              rel.freq=FALSE, 
              case_insensitive=TRUE,
              quietly=FALSE)
}

\arguments{
\item{corpus}{A quanteda corpus containing texts.}

\item{dic}{A quanteda dictionary object containing the terms to be searched.}

\item{group.var}{The variable in the corpus metadata to decompose the results. The default is NULL (all groups will be considered without desaggregation).}

\item{rel.freq}{Establishes whether the function calculates the relative frequency. The default is FALSE.}

\item{case_insensitive}{Indicates if the search will be insensitive to lower and uppercase letters. The default is TRUE.}

\item{quietly}{Logical. Indicates if the function hides the progress bar or not. The default is FALSE.}
}
\details{
The function countKeywords calculates number of times each term in a dictionary appears in a corpus. It allows both absolute and relative frequencies. Grouping variables contained in corpus metadata can also be employed to disaggregate the result by categories.  
}
\value{
A data.frame object containing the groups (all if the group.var argument is not specified), the categories of the dictionary, the keywords composing each category or coding level, and the frequency.
}
\examples{
\dontrun{
# Create a corpus object
library(quanteda)
cb <- corpus(spa.inaugural)

# use the dic.pol.es dictionary
dic <- dic.pol.es

# Generates the frequencies
d <- countKeywords(cb, 
                   dic)
                   
# Generates the relative frequencies 
# by President
d <- countKeywords(cb, 
                   dic,
                   group.var="President",
                   rel.freq=TRUE)
}
}