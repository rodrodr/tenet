\name{corTerms}
\alias{corTerms}
\title{Correlation Analysis of Words in a Corpus}
\description{
Performs a correlation analysis of the frequency of the words contained in a corpus.
}
\usage{
corTerms(corpus, 
        min.freq = 20,
        lang="es",
        method="spearman",
        r.lim=0,
        n.terms=50,
        remove.wordlist=NULL)
}

\arguments{
\item{corpus}{A quanteda corpus containing texts.}
\item{min.freq}{The minimum frequency to be included in the analysis. The default is 20.}
\item{lang}{The language for removing stopwords. The default is Spanish: "es".}
\item{method}{The correlation method to be employed in the correlation. The default is "spearman", the other options are "pearson", "kendall", and "yule" (this last one converts frequencies into binary data before calculating the correlation).}
\item{r.lim}{Indicates the degree of correlation that will be used to filter the values returned. The default is 0.}
\item{n.terms}{Indicates the number of terms to be returned by the function. The default is 50.}
\item{remove.wordlist}{List of words to be removed from the analysis alonside stopwords. The default is NULL.}
}
\details{
The function corTerm calculates the correlation coefficient for the frequency of words contained in a corpus object. It is designed to work with the corNet function which creates a sociogram of the links among words.  
}
\value{
A list containing two data.frame objects. The first, edges, is an edge list with three variables: term1, term2, and value. The second, vertices, indicates the feature, its frequency, and the number of documents in which it appears.
}
\examples{
# Create a corpus object
cb <- bra.inaugural

# Generates a list of correlations
ll <- corTerms( cb, 
                lang = "pt", 
                min.freq = 50, 
                n.terms = 50, 
                remove.wordlist = c("é",
                                "ser",
                                "fazer",
                                "cada",
                                "neste"))
}