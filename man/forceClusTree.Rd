\name{forceClusTree}
\alias{forceClusTree}
\title{Force Directed Hierarchical Cluster Tree Network}
\description{
Generates a Force Directed Hierarchical Cluster Tree Network Graph for corpus objects. 
}
\usage{
forceClusTree(corpus, 
              remove_punct=TRUE,
              lang = "es",
              palette = c("#DD8D29","#E2D200","#46ACC8","#E58601","#B40F20"),
              groupvar = NULL,
              width="100\%",
              height=580,
              link.width=2,
              maxRadius=30,
              BodyStrength=-10,
              include.text=FALSE,
              img.docvar=NULL,
              clust.method="euclidean",
              weight.scheme="logcount",
              elementId="chartdivclus")
}

\arguments{
\item{corpus}{A quanteda corpus object.}

\item{remove_punct}{Remove punctuation. The default is TRUE.}

\item{lang}{The language for removing stopwords. The default is Spanish: "es".}

\item{palette}{The color palette to be employed.}

\item{groupvar}{Metadata variable associated to the texts on a quanteda corpus object indicating groups or more aggregated categories.}

\item{width}{The width of the html panel. The default is 100\%.}

\item{height}{The height of the html panel in pixels. The default is 580.}

\item{link.width}{The width of the links connecting nodes in the network. The default is 2.}

\item{maxRadius}{The maximum radius of the nodes in the network. The default is 100.}

\item{BodyStrength}{The attraction force of nodes in the network.  The default is -10.}

\item{include.text}{Logical. Indicates if the function should include the text in the tooltip. It should be used only with small texts. Large texts would collapse the visualization. The default is FALSE.}

\item{img.docvar}{Indicates the name of the variable in the corpus metadata containing the url links to the images to be displayed in the tooltip. The default is NULL.}

\item{clust.method}{Indicates the method for calculating the text distance. The options are: "euclidean" (default), "manhattan", "maximum", "canberra", and "minkowski".}

\item{weight.scheme}{Indicates the method for calculating the weights for pondering the significance of words in each text. The options are: "logcount" (default), "count", "prop", "propmax", "boolean", "augmented", "logave". See quateda's dfm_weight for more information on how each scheme is calculated.}

\item{elementId}{Name of the div element employed to contain the graph. It is useful when you use the same type of graph multiple times in the same markdown page, for instance. The default is "chartdivclus".}

}
\details{
The function generates a Force Directed Hierarchical Cluster Tree Network Graph for quanteda corpuses. It calculates the similarity among texts and organizes them according to a tree structure. The interactive traits of the chart allow users to explore these relationships with more depth.}
\value{
A chart representing the similarity of texts or the html source code generated.
}
\examples{
\dontrun{
# Retrieve a corpus of text 
tx <- quanteda::data_corpus_inaugural

# Create the graph for the inaugural discourses
forceClusTree(tx, lang="en", maxRadius = 50)
}
}
