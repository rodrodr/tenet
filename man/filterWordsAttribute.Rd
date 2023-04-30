\name{filterWordsAttribute}
\alias{filterWordsAttribute}
\title{Filter Words from a Corpus by Attribute}
\description{
The function extracts the relative position of an attribute associated to the metadata for each text of a corpus object. 
}
\usage{
filterWordsAttribute(corpus,
            docvar,
            value,
            aggr.by.var)
}

\arguments{
\item{corpus}{A quanteda corpus object.}

\item{docvar}{Metadata variable associated to the texts on a quanteda corpus object.}

\item{value}{List of values contained in the metadata variable (docvar).}

\item{aggr.by.var}{Establish the grouping variable to be used in the creation of a new aggregated corpus.}
}
\details{
The function searches for words based on values from a metadata variable and allows new aggregation of data according to another grouping variable. It helps to identify actors, institutions, attributes presented in the text and highlight their relative position in a more aggregated perspective.}
\value{
A data.frame containing the words retreived, their relative position in each text and the grouping variable of the new corpus.
}
\examples{
\dontrun{
# Retrieve a corpus of text 
tx <- quanteda::data_corpus_inaugural

# find the relative position of keywords
filterWordsAttribute(corpus=tx, 
                     docvar="President", 
                     value=c("Nixon","Bush",
                              "Kennedy","Roosevelt"), 
                    aggr.by.var = "Party")
}
}
