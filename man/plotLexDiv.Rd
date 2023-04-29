\name{plotLexDiv}
\alias{plotLexDiv}
\title{Lexical Dispersion Plot}
\description{
The function creates a lexical dispersion plot for corpus objects based on keywords or dictionaries. 
}
\usage{
plotLexDiv(corpus,
           keywords,
           docvar=NULL,         
           value=NULL,
           aggr.by.var=NULL,
           rem.accent = TRUE,
           rem.punct = TRUE,
           case.insensitive = TRUE,
           lang = "es", 
           title="Lexical Dispersion Plot", 
           caption="Own elaboration.",
           subtitle="Keywords",
           legend.title="Group",
           legend.rows=1,
           palette="Royal1",
           custom.colors=NULL,
           panel.bg.fill="grey98",
           hline.color="white",
           hline.width=0.5,
           na.rm=FALSE)
}

\arguments{
\item{corpus}{A quanteda corpus object.}

\item{keywords}{Keywords or dictionary employed to search for terms in texts.}

\item{docvar}{Metadata variable associated to the texts on a quanteda corpus object.}

\item{value}{List of values contained in the metadata variable (docvar).}

\item{aggr.by.var}{Establish the grouping variable to be used in the creation of a new aggregated corpus.}

\item{rem.accent}{Remove accents. The default is TRUE.}

\item{rem.punct}{Remove punctuation. The default is TRUE.}

\item{case.insensitive}{Search for both upper and lowercase words. The default is TRUE.}

\item{lang}{The language for removing stopwords. The default is Spanish: "es".}

\item{title}{The title of the graph. The default ist "Lexical Dispersion Plot".}

\item{caption}{The caption, note or source of the graph. The default is "Own elaboration".}

\item{subtitle}{The subtitle of the graph. The default is "Keywords".}

\item{legend.title}{The title for the legend. The default is "Group".}

\item{legend.rows}{The number of rows organizing groups in the legend. The default is 1.}

\item{palette}{The color palette included in either RColorBrewer or wesanderson palettes. The default is "Royal1".}

\item{custom.colors}{The list of custom colors for a personalized color palette. The default is NULL.}

\item{panel.bg.fill}{The background color for the text panels. The default color is "grey98".}

\item{hline.color}{Horizontal line color. The default is "white".}

\item{hline.width}{Horizontal line width. The default is 0.5.}

\item{na.rm}{Logical. Removes texts that do not contain the keywords or categories indicated. The default is FAlSE.}
}
\details{
The function generates a Lexical Dispersion Plot for keywords, dictionaries or metadata variables. It represents the position of the selected keywords, dictionary categories, or metadata values in each text forming a corpus object.}
\value{
A chart representing the dispersion of terms or concepts.
}
\examples{
\dontrun{
# Retrieve a corpus of text 
tx <- quanteda::data_corpus_inaugural

# find the relative position of keywords
plotLexDiv(tx, keywords = c("democ","liber","freedom"))

# creates a dictionary
dic <- dictionary(list(
                Goverment=c("govern",
                            "executive",
                            "policy"),
                Nation=c("nation",
                         "homeland",
                         "patriot"),
                Democracy=c("democ",
                            "freedom",
                            "liberty",
                            "rights")
                ))

# Creates the graph                
plotLexDiv(tx, 
           keywords = dic, 
           palette = "Dark2")

# Aggregate the corpus according to party
plotLexDiv(tx, 
           docvar="President", 
           value=c("Nixon","Bush","Kennedy","Roosevelt"), 
           aggr.by.var = "Party")
}
}
