\name{plotSolar}
\alias{plotSolar}
\title{Solar Network Centrality Plot}
\usage{
plotSolar(
          data, 
          from="from", 
          to="to", 
          value="value",
          title="**Solar Network Centrality Plot**",
          subtitle="Relative Centrality and Rank Variation of Nodes in a Network.",
          caption="Own elaboration using network centrality ranks.",
          value.lab="Mentions",
          directed=FALSE,
          pos.jitter=0,
          palette=c("#FF0000",
                    "#00A08A",
                    "#F2AD00",
                    "#F98400",
                    "#5BBCD6"),
          center.col="red2",
          center.radius=0.05,
          orbit.line.col="grey90",
          orbit.line.type="dotted",
          orbit.line.width=0.5,
          arc.line.width=0.5,
          start.line.col="purple",
          start.line.width=0.5,
          start.line.type="dotted",
          legend.position="bottom",
          legend.justification="left"
          )
}

\description{
The \code{plotSolar} function generates a circular solar centrality plot to visualize the relative centrality and rank variation of nodes in a network.
}

\arguments{
\item{data}{A data.frame with three columns: from, to, and value.}

\item{from}{A character specifying the column name representing the source nodes in the network (default: "from").}

\item{to}{A character specifying the column name representing the target nodes in the network (default: "to").}

\item{value}{A character specifying the column name representing the node values (default: "value").}

\item{title}{
    A character specifying the plot title (default: "**Solar Network Centrality Plot**").
  }
  \item{subtitle}{
    A character specifying the plot subtitle (default: "Relative Centrality and Rank Variation of Nodes in a Network.").
  }
  \item{caption}{
    A character specifying the plot caption describing the data source and centrality measures used (default: "Own elaboration using network centrality ranks.").
  }
  \item{value.lab}{
    A character specifying the label for node values (default: "Mentions").
  }
  \item{directed}{
    A logical value indicating whether the network is directed (default: FALSE).
  }
  \item{pos.jitter}{
    A numeric value specifying the amount of jitter for node positions (default: 0).
  }
  \item{palette}{
    A vector of color codes specifying the node colors (default: a palette of five colors: "#FF0000", "#00A08A", "#F2AD00", "#F98400", and "#5BBCD6").
  }
  \item{center.col}{
    A character specifying the color of the central node (default: "red2").
  }
  \item{center.radius}{
    A numeric value specifying the radius of the central node (default: 0.05).
  }
  \item{orbit.line.col}{
    A character specifying the color of the orbit lines (default: "grey90").
  }
  \item{orbit.line.type}{
    A character specifying the type of orbit lines (default: "dotted").
  }
  \item{orbit.line.width}{
    A numeric value specifying the width of orbit lines (default: 0.5).
  }
    \item{arc.line.width}{
    A numeric value specifying the width of the lines representing orbit arcs (default: 0.5).
  }
  \item{start.line.col}{
    A character specifying the color of the starting lines (default: "purple").
  }
  \item{start.line.width}{
    A numeric value specifying the width of starting lines (default: 0.5).
  }
  \item{start.line.type}{
    A character specifying the type of starting lines (default: "dotted").
  }
  \item{legend.position}{
    A character specifying the position of the legend (default: "bottom").
  }
  \item{legend.justification}{
    A character specifying the justification of the legend (default: "left").
  }

}
\details{
  The \code{plotSolar} function takes a network represented as a data frame and calculates centrality measures (degree, closeness, betweenness, eigenvector, and PageRank) for the nodes. It then generates a circular plot resembling a solar system, where nodes are positioned radially based on their centrality ranks. The size and color of nodes, as well as the color and style of lines, can be customized using the function arguments.
}
\value{
  The function returns a ggplot2 plot object.
}

\examples{
\dontrun{
# Prepares the data
library(quanteda)
library(tenet)

# Creates a corpus from speeches
cp <- corpus(spa.inaugural)

# Reshape the corpus according to 
# sentences
cs <- corpus_reshape(cp, "sentences")

# Calculates the frequency of terms
# contained in the dic.pol.es dictionary
d1 <- matchCodes(cs, 
                 dic.pol.es, 
                 level = 2, 
                 quietly=TRUE)

# Order the results
d1 <- d1[order(d1$value, decreasing = T),]

# Plots the chart
plotSolar(
          d1, 
          pos.jitter = 0.01, 
          from = "term1", 
          to = "term2", 
          value = "value")
}
}
