\name{plotKeyness}
\alias{plotKeyness}
\title{Identify salient keywords in reference texts.}
\description{
Creates a plot identifying the most salient words from a reference text compared to other texts in a corpus.
}
\usage{
plotKeyness(corpus,
            group.var=NULL,
            ref.cat,
            p.value=0.01,
            type="chi2",
            palette=c("red3","goldenrod1","dodgerblue3"),
            remove.punct=TRUE,
            remove.number=TRUE,
            remove.stopwords=TRUE,
            use.stem=FALSE,
            use.bigrams=FALSE,
            label.dots=TRUE,
            gray.area=0,
            exclude.zeros=FALSE,
            lang="es",
            title="Chi-Square vs. Log Frequency",
            title.text.size=14,
            interactive=TRUE,
            return.data=FALSE)
}

\arguments{

  \item{corpus}{A quanteda corpus object with the texts to analyze.}
  
  \item{group.var}{A character vector with the name of the grouping variable in the corpus (in the case that the corpus is not already grouped).}

  \item{ref.cat}{A character string with the name of the reference category (one of the values of the grouping variable).}
  
  \item{p.value}{A numeric value with the significance level for the chi-square test. The default is 0.01 (99\% confidence).}
  
  \item{type}{A character string with the type of test to use. Options are "chi2" for the chi-square test, "lr" for likelyhood ratio, and "log" for the log-odds ratio of the chi2 results. The default is "chi2"}

  \item{palette}{A character vector with the colors to use in the plot. The first color is for the reference category, the second for the other categories, and the third for the gray area. The default colors are "red3","goldenrod1", and "dodgerblue3".}
  
  \item{remove.punct}{A logical value indicating if punctuation marks should be removed from the texts. The default is TRUE.}
  
  \item{remove.number}{A logical value indicating if numbers should be removed from the texts. The default is TRUE.}
  
  \item{remove.stopwords}{A logical value indicating if stopwords should be removed from the texts. The default is TRUE.}
  
  \item{use.stem}{A logical value indicating if the texts should be stemmed. The default is FALSE.}
  
  \item{use.bigrams}{A logical value indicating if bigrams should be used in the analysis. The default is FALSE.}
  
  \item{label.dots}{A logical value indicating if the words should be labeled in the plot. The default is TRUE.}
  
  \item{gray.area}{A numeric value indicating the value for establishing the bandwidth of the gray area in the plot. The default is 0 (no gray area).}
  
  \item{exclude.zeros}{A logical value indicating if words with zero frequency in the reference category should be excluded from the analysis. The default is FALSE.}
  
  \item{lang}{A character string with the language of the texts. The default is "es" (Spanish).}
  
  \item{title}{A character string with the title of the plot. The default is "Chi-Square vs. Log Frequency".}
  
  \item{title.text.size}{A numeric value with the size of the title text. The default is 14.}
  
  \item{interactive}{A logical value indicating if the plot should be interactive. The default is TRUE.}
  
  \item{return.data}{A logical value indicating if the data used to create the plot should be returned. The default is FALSE.}

}
\details{
The function employs the function textstat.keyness from the quanteda packages to calculate the salience of the words from a reference text compared to others and plot it against the log frequency of the term in the corpus scaled as a 0-1 interval. Therefore, the more distant from zero in both axes, the more salient the term is in the reference text. Terms above zero are particularly employed in the reference text while those below zero are absent in a greater degree when their prevalence is compared to other texts in the corpus.
}
\value{
A ggplot2 compatible scatter plot representing the scaled log frequency of a term in X-axis and the selected statistics in the Y-axis or a data.frame object containing the statistics.
}
\examples{
\dontrun{

# Selects the session no. 124 of the Spanish parliament
# discussing the law againgst sexual violence.
spa <- spa.sessions[spa.sessions$session.number==124,]

# Aggregate Spanish parliamentary speeches
# by party
re <- aggregate(list(text=spa$speech.text), 
                by=list(rep.party=spa$rep.party),
                FUN=paste, 
                collapse="\n")

# Create a corpus object with the speeches
cp <- corpus(re)

# Group the corpus by party
ci <- corpus_group(cp, groups = rep.party)

# Plot the keyness (log-odds ratio) of the words 
# in the speeches
plotKeyness(corpus = ci,
            type = "log", 
            ref.cat = "Podemos", 
            title = "")

}
}
