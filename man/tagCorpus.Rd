\name{tagCorpus}
\alias{tagCorpus}
\title{Corpus Tagger}
\description{
Tags sentences, paragraphs or other textual units according to categories contained in a dictionary.
}
\usage{
tagCorpus(corpus, 
          dic,
          reshape.dic=TRUE,
          reshape.to="paragraphs",
          palette = c("#DD8D29","#E2D200","#46ACC8","#E58601","#B40F20"), 
          bright = 130,
          pagination = TRUE, 
          defaultPageSize=10,
          show.details=TRUE,
          case.insensitive=TRUE,
          remove.accent=TRUE,
          filter=FALSE,
          data.return=FALSE,
          quietly=TRUE)
}

\arguments{
\item{corpus}{A quanteda corpus containing one or more documents.}
\item{dic}{The dictionary to be used to highlight or tag the submitted text.}
\item{reshape.dic}{Logical. Indicates whether the corpus will be reshaped or not. The default is TRUE.}
\item{reshape.to}{New document units in which the corpus will be recast. The default is "paragraphs". The other options are "documents" and "sentences".}
\item{palette}{The color palette for the text to be highlighted.}
\item{bright}{The cutoff value for the color brightness to define if a label text will be white or black. The default is 130.}
\item{pagination}{Indicates whether the table will organized according to equal-sized pages. The default is TRUE.}
\item{defaultPageSize}{Default page size (number of elements) for the table. The default is 10.}
\item{show.details}{Logical. Shows the order of the text, the document name, and statistics of terms and categories. The default is TRUE.}
\item{case.insensitive}{Logical. Indicates wether the case should be taken into account or not. The default is TRUE.}
\item{remove.accent}{Logical. Indicates wether the all accents should be removed. The default is TRUE.}
\item{filter}{Logical. Indicates wether those texts without categories or themes assigned should be filtered off. The default is FALSE.}
\item{data.return}{Logical. Defines if a data.frame with the classification of texts should be returned instead of an interactive table. The default is FALSE.}
\item{quietly}{Logical. Indicates wether the a progress bar should be used to inform users about the progress. This feature is useful for very large corpuses. The default is TRUE.}
}
\details{
The function tagCorpus finds and highlights words and expressions in a given text according to a dictionary. It was designed to help qualitative researchers to locate themes and issues in narrative texts and documents. Therefore, it is meant to be an exploratory tool for qualitative analysis and a step in a broader coding process of text.
}
\value{
The function creates an interactive table containing the order of the element in the corpus, the text, the main category found in the text, all categories, the number of matches, and the number of themes present in each element.
}
\examples{
\dontrun{
# Create a corpus
cp <- quanteda::corpus(spa.inaugural)

# Generate a dictionary with some keywords 
# associated to themes
dic <-quanteda::dictionary(
    list(libertad=c("libert","democr",
                    "derecho","libre"),
         cambio=c("cambi","reform","modern",
                  "transform","progres","futuro"),
         justicia=c("justicia","equidad"),
         patria=c("nacion","pueblo","espanol")
         )
)

# Create a table for sentences as 
# units of analysis
tagCorpus(cp, 
          dic=dic, 
          reshape.to = "sentences", 
          defaultPageSize = 5)
}
}