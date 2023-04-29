\name{wordtree}
\alias{wordtree}
\title{Word Tree Chart}
\description{
Creates a Google Chart's word tree for visualizing terms in context.
}
\usage{

wordtree(corpus, 
         keyword, 
         window=5, 
         direction="double", 
         rm.stop=FALSE, 
         lang="es", 
         url.return=FALSE, 
         height=900)
}

\arguments{
\item{corpus}{A quanteda corpus object to be used for the word tree.}
\item{keyword}{The term to be used as reference for the word tree.}
\item{direction}{The direction of the root. It can be either double (before and after the main targetWord), preffix (before), and suffix (after). The default is "double".}
\item{window}{The number of words to put before or after the main term. The default is 5.}
\item{rm.stop}{Logical. Defines if stopwords should be removed.The default is FALSE.}
\item{lang}{Language for removing the stopwords.The default is Spanish: "es".}
\item{url.return}{Defines whether the location of the html file should be returned.The default is TRUE.}
\item{height}{The height in pixels of the html page containing the chart. The default is 900 pixels.}
}
\details{
This function creates a word tree chart for exploring keywords in context.  
}
\value{
The result is an html file opened in a browser or its location in the local computer.
}
\examples{
\dontrun{
# Loads the dataset on US Presidential inaugural speeches
tx <- quanteda::data_corpus_inaugural

# Find the context for the word "democracy" in the texts
wordtree(tx, keyword="democracy")
}
}
