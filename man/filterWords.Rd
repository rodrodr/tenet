\name{filterWords}
\alias{filterWords}
\title{Filter Words from a Corpus}
\description{
The function extracts words and their relative position from a corpus object based either on a keyword list or a dictionary. 
}
\usage{
filterWords(corpus,
            keywords,
            rem.accent = TRUE,
            rem.punct = TRUE,
            case.insensitive = TRUE,
            lang = "es")
}

\arguments{
\item{corpus}{A quanteda corpus object.}

\item{keywords}{Keywords or dictionary employed to search for terms in texts.}

\item{rem.accent}{Remove accents. The default is TRUE.}

\item{rem.punct}{Remove punctuation. The default is TRUE.}

\item{case.insensitive}{Search for both upper and lowercase words. The default is TRUE.}

\item{lang}{The language for removing stopwords. The default is Spanish: "es".}
}
\details{
The function searches for terms using a keyword list or a dictionary and returns a list of words and their relative position in each text. These results are useful to be employed later in a Lexical Dispersion Plot.}
\value{
A data.frame containing the words retreived, their relative position in each text and the grouping variable, if existing.
}
\examples{
\dontrun{
# Retrieve a corpus of text 
tx <- quanteda::data_corpus_inaugural

# find the relative position of keywords
filterWords(corpus=tx, 
            keywords=c("democ","liber","govern"),
            lang="en")
}
}
