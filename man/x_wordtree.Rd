\name{x_wordtree}
\alias{x_wordtree}
\title{Word Tree Html Generator}
\description{
Creates the html code for a Google Chart's word tree for visualizing terms in context.
}
\usage{

x_wordtree(text,
         targetWord,
         direction="double",
         window=5,
         height=900)
}

\arguments{
\item{text}{Text to be used for the word tree.}
\item{targetWord}{Term to be used as reference for the word tree.}
\item{direction}{The direction of the root. It can be either double (before and after the main targetWord), preffix (before), and suffix (after). The default is "double".}
\item{window}{The number of words to put before or after the main term. The default is 5.}
\item{height}{The height in pixels of the html page containing the chart. The default is 900 pixels.}
}
\details{
This is an ancillary function to kwWordTree. It generates the final html code for the Google Chart's word tree. 
}
\value{
The result is the html code for the chart.
}
\examples{
\dontrun{
# Loads the dataset on US Presidential inaugural speeches
tx <- quanteda::data_corpus_inaugural

# Find the context for the word "democracy" in the texts
x_wordtree(tx, targetWord="democracy")
}
}
