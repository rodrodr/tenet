\name{tfRatio}
\alias{tfRatio}
\title{Term-Frequency Ratio}
\description{
Calculates the ratio between the frequency of a term in each document compared to its appearance in the whole corpus.
}
\usage{

tfRatio(text,
        keyword,
        return.selected=FALSE,
        threshold=0)
}

\arguments{
\item{text}{Collection of text documents.}
\item{keyword}{Keyword to be searched in the documents.}
\item{return.selected}{Logical. Should the ratios values be returned or only the index of documents accoding to the established threshold value.}
\item{threshold}{Defines the limits for selecting those texts were the keyword is particularly more frequent than most texts.}
}
\details{
Calculates the ratio of the frequency of a given term compared to its appearance in the whole corpus. This function is particularly useful for selecting texts according to themes or issues. It allows users to select only those documents containing the ideas for interest.
}
\value{
The function presents two possible values to be returned. The default is the frequency ratio. The ratio of a term will be high (much higher than 1) in those cases where few documents concentrate most of its occurence. It will be zero for the texts with no matches and will be between 0 and 1 for the documents where the frequency is below average.

If the option "return.selected=TRUE", the function will return all the values of the ratio above the limit established in the "threshold" argument. The default for this last parameter is to return all values above 0, i.e., all the texts where the term were obsersed at least once.

}
\examples{
\dontrun{
# Loads the dataset on US Presidential inaugural speeches
tx <- quanteda::data_corpus_inaugural

# Calculates the ratio for the root "democ" in all documents
tfRatio(text=tx, keyword="democ")

# Select just those with the double of occurence of the
# term than the average.
tfRatio(text=tx,
        keyword="democ",
        return.selected=TRUE,
        threshold=2)
}

}
